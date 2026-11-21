-- SPDX-FileCopyrightText: 2026 Moremi Vannak
--
-- SPDX-License-Identifier: MIT

-- | The part of the browser DOM that turndown reads, built from a zenacy-html
-- parse, and the HTML serialization of it.
--
-- Element names are the DOM @nodeName@: upper case for HTML elements, as
-- written for SVG and MathML ones.
module Text.Turndown.Dom
  ( -- * Nodes
    Node(..)
  , Element(..)
  , textContent
  , attribute
    -- * Parsing
  , parseRoot
  , parseBody
    -- * Serializing
  , outerHtml
  , renderNodes
  ) where

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List (find)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Zenacy.HTML qualified as Z

data Node
  = NText Text
  | NComment Text
  | NElement Element
  deriving stock (Eq, Show)

data Element = Element
  { name :: Text
  , attrs :: [(Text, Text)]
  , children :: [Node]
  }
  deriving stock (Eq, Show)

-- | The children of the root element turndown converts: the input wrapped in
-- @\<x-turndown id="turndown-root"\>@ and parsed as a whole document, like
-- turndown's @RootNode@ does, so the parser puts every element in one place.
parseRoot :: Text -> [Node]
parseRoot input =
  -- html > body > x-turndown: its children are at depth 4
  maybe [] (capDepth 4 . (.children)) $ findRoot (Z.htmlParseEasy wrapped)
  where
    wrapped = "<x-turndown id=\"turndown-root\">" <> escapeStrayLt input <> "</x-turndown>"
    findRoot = listToMaybe . go
    go = \case
      Z.HTMLDocument{htmlDocumentChildren} -> concatMap go htmlDocumentChildren
      el@Z.HTMLElement{htmlElementChildren}
        | isRoot el -> mapMaybe convert [el] >>= \case
            NElement e -> [e]
            _ -> []
        | otherwise -> concatMap go htmlElementChildren
      _ -> []
    isRoot el = any
      (\a -> Z.htmlAttrName a == "id" && Z.htmlAttrVal a == "turndown-root")
      (Z.htmlElementAttributes el)

-- | The children of the body of an HTML document or fragment.
parseBody :: Text -> [Node]
parseBody input =
  -- html > body: its children are at depth 3
  maybe [] (capDepth 3 . (.children)) $ listToMaybe $ go (Z.htmlParseEasy (escapeStrayLt input))
  where
    go = \case
      Z.HTMLDocument{htmlDocumentChildren} -> concatMap go htmlDocumentChildren
      el@Z.HTMLElement{htmlElementName, htmlElementChildren}
        | htmlElementName == "body" -> mapMaybe convert [el] >>= \case
            NElement e -> [e]
            _ -> []
        | otherwise -> concatMap go htmlElementChildren
      _ -> []

-- | Blink nests elements at most 512 deep: a deeper element is put next to
-- the others at that depth, its own children after it. Doing the same keeps
-- the output the browser's, and bounds turndown's work (each blockquote
-- prefixes all of its content again, so deep nesting costs depth times size).
capDepth :: Int -> [Node] -> [Node]
capDepth depth
  | depth < maxDepth = map \case
      NElement e -> NElement e{children = capDepth (depth + 1) e.children}
      node -> node
  | otherwise = concatMap flatten
  where
    maxDepth = 512
    flatten = \case
      NElement e -> NElement e{children = []} : concatMap flatten e.children
      node -> [node]

-- | A @\<@ that cannot open a tag as @&lt;@. In the HTML tokenizer's tag open
-- state anything but a letter, @!@, @/@ or @?@ makes the @\<@ text, which the
-- browser keeps and zenacy-html drops (e.g. @Name \<\<a href=...\>@).
escapeStrayLt :: Text -> Text
escapeStrayLt t = case T.splitOn "<" t of
  first : rest -> T.concat (first : map escaped rest)
  [] -> t
  where
    -- A piece that followed a @<@
    escaped piece =
      let opensMarkup = maybe False (\(c, _) -> isAsciiLetter c || c `elem` ("!/?" :: String)) (T.uncons piece)
      in (if opensMarkup then "<" else "&lt;") <> piece
    isAsciiLetter c = isAsciiLower c || isAsciiUpper c

convert :: Z.HTMLNode -> Maybe Node
convert = \case
  Z.HTMLText{htmlTextData} -> Just (NText htmlTextData)
  Z.HTMLComment{htmlCommentData} -> Just (NComment htmlCommentData)
  Z.HTMLElement{htmlElementName, htmlElementNamespace, htmlElementAttributes, htmlElementChildren} ->
    Just $ NElement Element
      { name = nodeName htmlElementNamespace htmlElementName
      , attrs = map (\a -> (Z.htmlAttrName a, Z.htmlAttrVal a)) htmlElementAttributes
      , children = mergeTexts (mapMaybe convert htmlElementChildren)
      }
  -- A template keeps its content in a fragment, not in its children
  Z.HTMLTemplate{htmlTemplateAttributes} ->
    Just $ NElement Element
      { name = "TEMPLATE"
      , attrs = map (\a -> (Z.htmlAttrName a, Z.htmlAttrVal a)) htmlTemplateAttributes
      , children = []
      }
  _ -> Nothing
  where
    nodeName ns n
      | ns == Z.HTMLNamespaceHTML = T.toUpper n
      | otherwise = n

-- | The browser parser never leaves two text nodes side by side
mergeTexts :: [Node] -> [Node]
mergeTexts = \case
  NText a : NText b : rest -> mergeTexts (NText (a <> b) : rest)
  n : rest -> n : mergeTexts rest
  [] -> []

-- | The DOM @textContent@: the text of every descendant text node.
textContent :: Node -> Text
textContent = \case
  NText t -> t
  NComment _ -> ""
  NElement e -> T.concat (map textContent e.children)

attribute :: Text -> Element -> Maybe Text
attribute n e = snd <$> find ((== n) . fst) e.attrs

-- | The DOM @outerHTML@ of an element, as the HTML serialization algorithm
-- writes it.
outerHtml :: Element -> Text
outerHtml e = renderNodes [NElement e]

renderNodes :: [Node] -> Text
renderNodes = T.concat . map render
  where
    render = \case
      NText t -> escapeText t
      NComment c -> "<!--" <> c <> "-->"
      NElement e ->
        let tag = T.toLower e.name
            open = "<" <> tag <> T.concat [" " <> k <> "=\"" <> escapeAttr v <> "\"" | (k, v) <- e.attrs] <> ">"
        in if tag `elem` voidTags
          then open
          else open <> renderNodes e.children <> "</" <> tag <> ">"
    escapeText = T.concatMap \case
      '&' -> "&amp;"
      '<' -> "&lt;"
      '>' -> "&gt;"
      '\x00A0' -> "&nbsp;"
      c -> T.singleton c
    escapeAttr = T.concatMap \case
      '&' -> "&amp;"
      '"' -> "&quot;"
      '<' -> "&lt;"
      '>' -> "&gt;"
      '\x00A0' -> "&nbsp;"
      c -> T.singleton c
    voidTags =
      [ "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta"
      , "source", "track", "wbr", "basefont", "bgsound", "frame", "keygen", "param"
      ]
