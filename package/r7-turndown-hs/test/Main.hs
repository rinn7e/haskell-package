-- SPDX-FileCopyrightText: 2026 Moremi Vannak
--
-- SPDX-License-Identifier: MIT

-- | Golden tests: for every @test/golden/\<options\>/\<name\>.md@, 'turndown'
-- with those options gives it from @test/golden/inputs/\<name\>.html@: the
-- output of turndown itself (written by @scripts/golden.mjs@ with that
-- folder's @options.json@).
module Main (main) where

import Control.Monad (forM)
import Data.List (isSuffixOf, sort)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory (listDirectory)
import System.FilePath (dropExtension, (</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Text.Turndown

goldenDir :: FilePath
goldenDir = "test/golden"

-- | The options of each folder, as its @options.json@ gives them to turndown
optionSets :: [(FilePath, Options)]
optionSets =
  [ ("default", defaultOptions)
  , ( "atx-fenced"
    , defaultOptions
        { headingStyle = Atx
        , codeBlockStyle = Fenced
        , bulletListMarker = "-"
        , emDelimiter = "*"
        , strongDelimiter = "__"
        , hr = "---"
        }
    )
  ]

main :: IO ()
main = do
  groups <- forM optionSets \(set, opts) -> do
    files <- sort . filter (".md" `isSuffixOf`) <$> listDirectory (goldenDir </> set)
    cases <- forM files \file -> do
      html <- T.readFile (goldenDir </> "inputs" </> dropExtension file <> ".html")
      expected <- T.readFile (goldenDir </> set </> file)
      pure $ testCase file (T.unpack (turndown opts html) @?= T.unpack expected)
    pure (testGroup set cases)
  defaultMain (testGroup "turndown" groups :: TestTree)
