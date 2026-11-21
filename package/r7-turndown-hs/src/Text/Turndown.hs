-- SPDX-FileCopyrightText: 2017 Dom Christie
-- SPDX-FileCopyrightText: 2014 Luc Thevenard
-- SPDX-FileCopyrightText: 2026 Moremi Vannak
--
-- SPDX-License-Identifier: MIT

-- | HTML to Markdown: a port of turndown 7.2.4
-- (<https://github.com/mixmark-io/turndown>) that gives the JavaScript
-- library's output, character for character.
--
-- It follows the JavaScript function by function: the same DOM (an HTML5
-- parse, see "Text.Turndown.Dom"), the same white space collapsing (adapted
-- from collapse-whitespace by Luc Thevenard, as in turndown), the same rules
-- in the same order, the same escaping and the same joining of blocks.
--
-- > import Text.Turndown (defaultOptions, turndown)
-- >
-- > turndown defaultOptions "<h1>Hello</h1><p>A <em>small</em> world</p>"
-- > -- "Hello\n=====\n\nA _small_ world"
--
-- Not ported: @linkStyle: 'referenced'@, @preformattedCode@, @keep@ /
-- @remove@ (a custom 'Rule' does what they do), and the @blankReplacement@,
-- @keepReplacement@ and @defaultReplacement@ options (their defaults are
-- built in).
module Text.Turndown
  ( -- * Converting
    turndown
    -- * Options
  , Options(..)
  , defaultOptions
  , HeadingStyle(..)
  , CodeBlockStyle(..)
    -- * Rules
  , Rule(..)
  , RuleContext(..)
    -- * Escaping
  , escapeMarkdown
  ) where

import Data.Char (isDigit, ord)
import Data.IntMap.Strict qualified as IM
import Data.List (foldl', mapAccumL, zipWith5)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR

import Text.Turndown.Dom (Element(..), Node(..), attribute, parseRoot, textContent)
import Text.Turndown.Whitespace (isJsSpace, jsTrim)

------------------------------------------------------------------------------
-- Options

-- | turndown's options. 'defaultOptions' are turndown's defaults.
data Options = Options
  { headingStyle :: HeadingStyle
  , hr :: Text
  , bulletListMarker :: Text
  , codeBlockStyle :: CodeBlockStyle
  , fence :: Text
  , emDelimiter :: Text
  , strongDelimiter :: Text
  , br :: Text
  -- | Escapes a text node (code is never escaped), like assigning
  -- @turndownService.escape@. 'escapeMarkdown' by default.
  , escape :: Text -> Text
  -- | Tried in order before the built-in rules, like @addRule@ (whose rules
  -- also come first, the last added first of all).
  , rules :: [Rule]
  }

data HeadingStyle = Setext | Atx
  deriving stock (Eq, Show)

data CodeBlockStyle = Indented | Fenced
  deriving stock (Eq, Show)

-- | @new TurndownService()@
defaultOptions :: Options
defaultOptions = Options
  { headingStyle = Setext
  , hr = "* * *"
  , bulletListMarker = "*"
  , codeBlockStyle = Indented
  , fence = "```"
  , emDelimiter = "_"
  , strongDelimiter = "**"
  , br = "  "
  , escape = escapeMarkdown
  , rules = []
  }

-- | A conversion rule, like turndown's @{ filter, replacement }@. A blank
-- element (no text, no void or meaningful descendant) never reaches a rule.
data Rule = Rule
  { matches :: RuleContext -> Element -> Bool
  -- | The Markdown of the element, from the Markdown of its content
  , replacement :: RuleContext -> Element -> Text -> Text
  }

-- | Where the element sits, for the rules that look around (turndown's rules
-- read @node.parentNode@ and the siblings).
data RuleContext = RuleContext
  { options :: Options
  , parent :: Element
  , previousSibling :: Maybe Node
  , nextSibling :: Maybe Node
  -- | Its index among the element children of its parent
  , elementIndex :: Int
  , isLastElementChild :: Bool
  -- | Inside a @CODE@ element
  , isCode :: Bool
  }

------------------------------------------------------------------------------
-- Converting

-- | Converts HTML to Markdown, like @turndownService.turndown(html)@.
--
-- zenacy-html can throw a lazy pure exception on some broken HTML (a
-- multibyte character fostered out of a @\<table\>@ in a way that splits its
-- bytes); a caller that must not fail forces the result and catches it.
turndown :: Options -> Text -> Text
turndown opts input
  | T.null input = ""
  | otherwise =
      let root = Element{name = "X-TURNDOWN", attrs = [], children = collapseWhitespace (parseRoot input)}
      in postProcess (process opts False root)

postProcess :: Text -> Text
postProcess =
  T.dropWhileEnd (\c -> c `elem` ("\t\r\n" :: String) || isJsSpace c)
    . T.dropWhile (`elem` ("\t\r\n" :: String))

------------------------------------------------------------------------------
-- Element lists

blockElements, voidElements, meaningfulWhenBlankElements :: [Text]
blockElements =
  [ "ADDRESS", "ARTICLE", "ASIDE", "AUDIO", "BLOCKQUOTE", "BODY", "CANVAS", "CENTER", "DD", "DIR"
  , "DIV", "DL", "DT", "FIELDSET", "FIGCAPTION", "FIGURE", "FOOTER", "FORM", "FRAMESET", "H1", "H2"
  , "H3", "H4", "H5", "H6", "HEADER", "HGROUP", "HR", "HTML", "ISINDEX", "LI", "MAIN", "MENU", "NAV"
  , "NOFRAMES", "NOSCRIPT", "OL", "OUTPUT", "P", "PRE", "SECTION", "TABLE", "TBODY", "TD", "TFOOT"
  , "TH", "THEAD", "TR", "UL"
  ]
voidElements =
  [ "AREA", "BASE", "BR", "COL", "COMMAND", "EMBED", "HR", "IMG", "INPUT", "KEYGEN", "LINK", "META"
  , "PARAM", "SOURCE", "TRACK", "WBR"
  ]
meaningfulWhenBlankElements =
  ["A", "TABLE", "THEAD", "TBODY", "TFOOT", "TH", "TD", "IFRAME", "SCRIPT", "AUDIO", "VIDEO"]

isBlock, isVoid, isMeaningfulWhenBlank :: Element -> Bool
isBlock e = e.name `elem` blockElements
isVoid e = e.name `elem` voidElements
isMeaningfulWhenBlank e = e.name `elem` meaningfulWhenBlankElements

-- | Whether a descendant (not the element itself) has one of the names
hasDescendant :: [Text] -> Element -> Bool
hasDescendant names e = any go e.children
  where
    go = \case
      NElement c -> c.name `elem` names || any go c.children
      _ -> False

------------------------------------------------------------------------------
-- collapseWhitespace

-- A node numbered for the white space pass; a PRE keeps its subtree as is
data NumNode
  = NumText Int Text
  | NumComment
  | NumElement Element [NumNode]
  | NumPre Element

data Visit = VisitText Int Text | VisitElement Element

data Collapse = Collapse
  { prevText :: Maybe Int
  , keepLeadingWs :: Bool
  , texts :: IM.IntMap Text
  }

-- | turndown's @collapseWhitespace@ over the root's children: the same walk in
-- document order (an element with children is visited on the way in and on
-- the way out, a PRE is not entered), the same state, and the same edits
-- (a text node may lose its last space later, when a block follows it).
collapseWhitespace :: [Node] -> [Node]
collapseWhitespace nodes =
  let (_, numbered) = number 0 nodes
      final = finish (foldl' visit (Collapse Nothing False IM.empty) (visits numbered))
  in rebuild final.texts numbered
  where
    number :: Int -> [Node] -> (Int, [NumNode])
    number = mapAccumL \i -> \case
      NText t -> (i + 1, NumText i t)
      NComment _ -> (i, NumComment)
      NElement e
        | e.name == "PRE" -> (i, NumPre e)
        | otherwise -> let (i', kids) = number i e.children in (i', NumElement e kids)

    visits :: [NumNode] -> [Visit]
    visits = concatMap \case
      NumText i t -> [VisitText i t]
      NumComment -> []
      NumPre e -> [VisitElement e]
      NumElement e kids
        | null kids -> [VisitElement e]
        | otherwise -> VisitElement e : visits kids <> [VisitElement e]

    visit :: Collapse -> Visit -> Collapse
    visit st = \case
      VisitText i raw ->
        let text0 = collapseRuns raw
            prevEndsWithSpace = maybe True (\p -> " " `T.isSuffixOf` IM.findWithDefault "" p st.texts) st.prevText
            text
              | prevEndsWithSpace && not st.keepLeadingWs && " " `T.isPrefixOf` text0 = T.drop 1 text0
              | otherwise = text0
        in if T.null text
          then st
          else st{texts = IM.insert i text st.texts, prevText = Just i}
      VisitElement e
        | isBlock e || e.name == "BR" ->
            st{texts = trimPrev st, prevText = Nothing, keepLeadingWs = False}
        | isVoid e || e.name == "PRE" -> st{prevText = Nothing, keepLeadingWs = True}
        | isJust st.prevText -> st{keepLeadingWs = False}
        | otherwise -> st

    trimPrev :: Collapse -> IM.IntMap Text
    trimPrev st = case st.prevText of
      Just p -> IM.adjust dropOneTrailingSpace p st.texts
      Nothing -> st.texts

    finish :: Collapse -> Collapse
    finish st = case st.prevText of
      Just p ->
        let t = dropOneTrailingSpace (IM.findWithDefault "" p st.texts)
        in st{texts = if T.null t then IM.delete p st.texts else IM.insert p t st.texts}
      Nothing -> st

    dropOneTrailingSpace t = maybe t fst (T.unsnoc t >>= \(s, c) -> if c == ' ' then Just (s, c) else Nothing)

    -- @replace(/[ \r\n\t]+/g, ' ')@
    collapseRuns =
      T.concat
        . map (\g -> if T.all isCollapsible g then " " else g)
        . T.groupBy (\a b -> isCollapsible a == isCollapsible b)
      where
        isCollapsible c = c `elem` (" \r\n\t" :: String)

    rebuild :: IM.IntMap Text -> [NumNode] -> [Node]
    rebuild final = concatMap \case
      NumText i _ -> maybe [] (pure . NText) (IM.lookup i final)
      NumComment -> []
      NumPre e -> [NElement e]
      NumElement e kids -> [NElement e{children = rebuild final kids}]

------------------------------------------------------------------------------
-- process

process :: Options -> Bool -> Element -> Text
process opts code parent =
  joinAll $ zipWith5 replacementFor [0 :: Int ..] kids previous following elementIndices
  where
    kids = parent.children
    previous = Nothing : map Just kids
    following = drop 1 (map Just kids) <> [Nothing]
    elementIndices = snd $ mapAccumL (\n k -> case k of NElement _ -> (n + 1, n); _ -> (n, n)) 0 kids
    lastElementPos = foldl' (\acc (i, k) -> case k of NElement _ -> i; _ -> acc) (-1) (zip [0 :: Int ..] kids)
    replacementFor i node prev next ei =
      let ctx = RuleContext
            { options = opts
            , parent
            , previousSibling = prev
            , nextSibling = next
            , elementIndex = ei
            , isLastElementChild = i == lastElementPos
            , isCode = code
            }
      in case node of
        NText t -> if code then t else opts.escape t
        NComment _ -> ""
        NElement e -> replacementForNode ctx e

replacementForNode :: RuleContext -> Element -> Text
replacementForNode ctx e =
  let content0 = process ctx.options (e.name == "CODE" || ctx.isCode) e
      (leading, trailing) = flankingWhitespace ctx e
      content = if T.null leading && T.null trailing then content0 else jsTrim content0
  in leading <> ruleFor ctx e content <> trailing

-- | turndown's @join@ folded over the replacements of an element's children:
-- the newlines where two meet are the most either side has, at most two.
-- The output is kept as chunks without its trailing newlines, plus how many
-- there are, so a step neither measures nor copies the output so far (which
-- made a long run of siblings quadratic).
joinAll :: [Text] -> Text
joinAll = finish . foldl' step ([], 0)
  where
    step (chunks, pending) replacement =
      let lead = T.length (T.takeWhile (== '\n') replacement)
          rest = T.drop lead replacement
          separator = T.take (max pending lead) "\n\n"
      in if T.null rest
        then (chunks, T.length separator)
        else
          let trail = T.length (T.takeWhileEnd (== '\n') rest)
          in (T.dropEnd trail rest : separator : chunks, trail)
    finish (chunks, pending) = T.concat (reverse chunks) <> T.replicate pending "\n"

------------------------------------------------------------------------------
-- Node: blank and flanking white space

isBlank :: Element -> Bool
isBlank e =
  not (isVoid e)
    && not (isMeaningfulWhenBlank e)
    && T.all isJsSpace (textContent (NElement e))
    && not (hasDescendant voidElements e)
    && not (hasDescendant meaningfulWhenBlankElements e)

flankingWhitespace :: RuleContext -> Element -> (Text, Text)
flankingWhitespace ctx e
  | isBlock e = ("", "")
  | otherwise =
      let s = textContent (NElement e)
          (leadAll, rest) = T.span isJsSpace s
          (leadAscii, leadNonAscii) = T.span isAsciiWs leadAll
          (trailAscii, trailNonAscii)
            | T.null rest = ("", "")
            | otherwise =
                let trailAll = T.takeWhileEnd isJsSpace s
                    ascii = T.takeWhileEnd isAsciiWs trailAll
                in (ascii, T.dropEnd (T.length ascii) trailAll)
          leading
            | not (T.null leadAscii) && flanked T.isSuffixOf ctx.previousSibling = leadNonAscii
            | otherwise = leadAll
          trailing
            | not (T.null trailAscii) && flanked T.isPrefixOf ctx.nextSibling = trailNonAscii
            | otherwise = trailNonAscii <> trailAscii
      in (leading, trailing)
  where
    isAsciiWs c = c `elem` (" \t\r\n" :: String)
    flanked test = \case
      Just (NText t) -> " " `test` t
      Just (NElement s) | not (isBlock s) -> " " `test` textContent (NElement s)
      _ -> False

------------------------------------------------------------------------------
-- Rules

-- | turndown's @rules.forNode@: the blank rule, the custom rules, the built-in
-- rules, then the default rule.
ruleFor :: RuleContext -> Element -> Text -> Text
ruleFor ctx e content
  | isBlank e = if isBlock e then "\n\n" else ""
  | Just r <- listToMaybe [r | r <- ctx.options.rules, r.matches ctx e] = r.replacement ctx e content
  | otherwise = builtIn ctx e content

builtIn :: RuleContext -> Element -> Text -> Text
builtIn ctx e content
  | lname == "p" = "\n\n" <> content <> "\n\n"
  | lname == "br" = opts.br <> "\n"
  | Just level <- headingLevel = heading level
  | lname == "blockquote" = "\n\n" <> prefixLines "> " (trimNewlines content) <> "\n\n"
  | lname `elem` ["ul", "ol"] =
      if ctx.parent.name == "LI" && ctx.isLastElementChild
        then "\n" <> content
        else "\n\n" <> content <> "\n\n"
  | lname == "li" = listItem
  | e.name == "PRE", NElement c : _ <- e.children, c.name == "CODE" = codeBlock c
  | lname == "hr" = "\n\n" <> opts.hr <> "\n\n"
  | e.name == "A", Just href <- attribute "href" e, not (T.null href) =
      let title = escapeLinkTitle (cleanAttribute (attribute "title" e))
          titlePart = if T.null title then "" else " \"" <> title <> "\""
      in "[" <> content <> "](" <> escapeLinkDestination href <> titlePart <> ")"
  | lname `elem` ["em", "i"] = delimit opts.emDelimiter
  | lname `elem` ["strong", "b"] = delimit opts.strongDelimiter
  | e.name == "CODE" && not (ctx.parent.name == "PRE" && noSiblings) = code
  | lname == "img" = image
  | otherwise = if isBlock e then "\n\n" <> content <> "\n\n" else content
  where
    opts = ctx.options
    lname = T.toLower e.name
    noSiblings = not (isJust ctx.previousSibling || isJust ctx.nextSibling)

    headingLevel = case T.unpack lname of
      ['h', d] | d >= '1' && d <= '6' -> Just (ord d - ord '0')
      _ -> Nothing

    -- A setext underline is as long as the content in UTF-16 code units,
    -- JavaScript's length
    heading level
      | opts.headingStyle == Setext && level < 3 =
          let underline = T.replicate (utf16Length content) (if level == 1 then "=" else "-")
          in "\n\n" <> content <> "\n" <> underline <> "\n\n"
      | otherwise = "\n\n" <> T.replicate level "#" <> " " <> content <> "\n\n"

    listItem =
      let prefix
            | ctx.parent.name == "OL" =
                let n = case attribute "start" ctx.parent of
                      Just start | not (T.null start) -> jsNumberPlus start ctx.elementIndex
                      _ -> T.pack (show (ctx.elementIndex + 1))
                in n <> ".  "
            | otherwise = opts.bulletListMarker <> "   "
          isParagraph = "\n" `T.isSuffixOf` content
          content' = trimNewlines content <> (if isParagraph then "\n" else "")
          indented = T.replace "\n" ("\n" <> T.replicate (T.length prefix) " ") content'
      in prefix <> indented <> (if isJust ctx.nextSibling then "\n" else "")

    codeBlock c = case opts.codeBlockStyle of
      Indented -> "\n\n    " <> T.replace "\n" "\n    " (textContent (NElement c)) <> "\n\n"
      Fenced ->
        let className = fromMaybe "" (attribute "class" c)
            language = case T.breakOn "language-" className of
              (_, rest) | not (T.null rest) -> T.takeWhile (not . isJsSpace) (T.drop 9 rest)
              _ -> ""
            codeText = textContent (NElement c)
            fenceChar = T.take 1 opts.fence
            -- @/^<fence char>{3,}/gm@: every run at a line start makes the fence longer
            longestFence = maximum (0 : [T.length l | l <- map (T.takeWhile ((== fenceChar) . T.singleton)) (T.lines codeText), T.length l >= 3])
            fenceSize = if longestFence >= 3 then longestFence + 1 else 3
            fenceText = T.replicate fenceSize fenceChar
            body = maybe codeText fst (T.unsnoc codeText >>= \(s, ch) -> if ch == '\n' then Just (s, ch) else Nothing)
        in "\n\n" <> fenceText <> language <> "\n" <> body <> "\n" <> fenceText <> "\n\n"

    delimit d = if T.null (jsTrim content) then "" else d <> content <> d

    code
      | T.null content = ""
      | otherwise =
          let c = T.replace "\r" " " (T.replace "\n" " " (T.replace "\r\n" " " content))
              extraSpace = if "`" `T.isPrefixOf` c || "`" `T.isSuffixOf` c || spacedInside c then " " else ""
              runs = filter ("`" `T.isPrefixOf`) (T.group c)
              delimiter = until (`notElem` runs) (<> "`") "`"
          in delimiter <> extraSpace <> c <> extraSpace <> delimiter

    -- @/^ .*?[^ ].* $/@
    spacedInside c =
      T.length c >= 3
        && " " `T.isPrefixOf` c
        && " " `T.isSuffixOf` c
        && T.any (/= ' ') (T.drop 1 (T.dropEnd 1 c))
        && not (T.any (`elem` ("\n\r\x2028\x2029" :: String)) c)

    image =
      let alt = escapeMarkdown (cleanAttribute (attribute "alt" e))
          src = escapeLinkDestination (fromMaybe "" (attribute "src" e))
          title = cleanAttribute (attribute "title" e)
          titlePart = if T.null title then "" else " \"" <> escapeLinkTitle title <> "\""
      in if T.null src then "" else "![" <> alt <> "](" <> src <> titlePart <> ")"

-- | @Number(start) + index@ as JavaScript prints it
jsNumberPlus :: Text -> Int -> Text
jsNumberPlus start index =
  let s = jsTrim start
  in if
    | T.null s                                -> T.pack (show index)
    | Right (n, "") <- TR.signed TR.decimal s -> T.pack (show (n + toInteger index))
    | otherwise                               -> "NaN"

utf16Length :: Text -> Int
utf16Length = T.foldl' (\n c -> n + if ord c > 0xFFFF then 2 else 1) 0

trimNewlines :: Text -> Text
trimNewlines = T.dropWhileEnd (== '\n') . T.dropWhile (== '\n')

-- | @replace(/^/gm, prefix)@: before the text and after every line terminator
prefixLines :: Text -> Text -> Text
prefixLines prefix t = prefix <> T.concatMap (\c -> if isLineTerminator c then T.singleton c <> prefix else T.singleton c) t
  where
    isLineTerminator c = c `elem` ("\n\r\x2028\x2029" :: String)

-- | @replace(/(\n+\s*)+/g, '\n')@: a newline and all white space after it
cleanAttribute :: Maybe Text -> Text
cleanAttribute = maybe "" go
  where
    go t = case T.breakOn "\n" t of
      (before, rest)
        | T.null rest -> before
        | otherwise -> before <> "\n" <> go (T.dropWhile isJsSpace rest)

escapeLinkDestination :: Text -> Text
escapeLinkDestination destination =
  let escaped = T.concatMap (\c -> if c `elem` ("<>()" :: String) then T.pack ['\\', c] else T.singleton c) destination
  in if " " `T.isInfixOf` escaped then "<" <> escaped <> ">" else escaped

escapeLinkTitle :: Text -> Text
escapeLinkTitle = T.replace "\"" "\\\""

------------------------------------------------------------------------------
-- Escaping

-- | turndown's @escapeMarkdown@, the default 'escape': its regular
-- expressions in order. Those with @^@ have no @m@ flag, so they look at the
-- start of the text only.
escapeMarkdown :: Text -> Text
escapeMarkdown =
  numberedList
    . T.replace "_" "\\_"
    . startWith ">" ("\\" <>)
    . T.replace "]" "\\]"
    . T.replace "[" "\\["
    . startWith "~~~" ("\\" <>)
    . T.replace "`" "\\`"
    . atxHeading
    . startWith "=" ("\\" <>)
    . startWith "+ " ("\\" <>)
    . startWith "-" ("\\" <>)
    . T.replace "*" "\\*"
    . T.replace "\\" "\\\\"
  where
    startWith p f t = if p `T.isPrefixOf` t then f t else t
    -- @/^(#{1,6}) /@
    atxHeading t =
      let n = T.length (T.takeWhile (== '#') t)
      in if n >= 1 && n <= 6 && T.take 1 (T.drop n t) == " " then "\\" <> t else t
    -- @/^(\d+)\. /@
    numberedList t =
      let digits = T.takeWhile isDigit t
          rest = T.drop (T.length digits) t
      in if not (T.null digits) && ". " `T.isPrefixOf` rest then digits <> "\\" <> rest else t
