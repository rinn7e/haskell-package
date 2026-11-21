# turndown-hs

Convert HTML to Markdown in Haskell, with exactly the output of
[turndown](https://github.com/mixmark-io/turndown) 7.2.4, the JavaScript
library.

`turndown-hs` is a port of turndown, function by function. It parses HTML into
the same DOM as a browser does, collapses white space the same way, runs the
same rules in the same order, and escapes and joins blocks the same way. So a
Haskell server makes the same Markdown, character for character, as turndown
does in a browser. A golden test suite checks this against turndown itself.

## Features

- The same output as turndown 7.2.4, checked by golden tests made with turndown
  in Chromium.
- The options of turndown, with the same defaults.
- Custom rules, tried before the built-in ones (like `addRule`).
- A custom escape function (like assigning `turndownService.escape`).
- Pure: `turndown :: Options -> Text -> Text`.

## Installation

The package is named `turndown` and is not on Hackage yet. Add it as a
dependency from this repository.

With Stack, in `stack.yaml`:

```yaml
extra-deps:
  - github: rinn7e/haskell-package
    commit: <commit>
    subdirs:
      - package/turndown-hs
  - zenacy-html-2.1.2
```

With cabal, in `cabal.project`:

```cabal
source-repository-package
  type: git
  location: https://github.com/rinn7e/haskell-package.git
  tag: <commit>
  subdir: package/turndown-hs
```

Then add `turndown` to the `build-depends` (or the `dependencies` of
`package.yaml`) of your package.

## Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Text.Turndown (defaultOptions, turndown)

markdown :: Text
markdown = turndown defaultOptions "<h1>Hello</h1><p>A <em>small</em> world</p>"
-- "Hello\n=====\n\nA _small_ world"
```

### Options

`defaultOptions` is `new TurndownService()`. Change any field to set an option:

```haskell
import Text.Turndown

options :: Options
options = defaultOptions
  { headingStyle = Atx
  , codeBlockStyle = Fenced
  , bulletListMarker = "-"
  }

-- turndown options "<h2>Install</h2><ul><li>one</li><li>two</li></ul>"
-- "## Install\n\n-   one\n-   two"
```

| Field | turndown option | Default |
| --- | --- | --- |
| `headingStyle` | `headingStyle` | `Setext` (or `Atx`) |
| `hr` | `hr` | `"* * *"` |
| `bulletListMarker` | `bulletListMarker` | `"*"` |
| `codeBlockStyle` | `codeBlockStyle` | `Indented` (or `Fenced`) |
| `fence` | `fence` | `` "```" `` |
| `emDelimiter` | `emDelimiter` | `"_"` |
| `strongDelimiter` | `strongDelimiter` | `"**"` |
| `br` | `br` | `"  "` (two spaces) |
| `escape` | `turndownService.escape` | `escapeMarkdown` |
| `rules` | `addRule` | `[]` |

### Custom rules

A `Rule` is the `{ filter, replacement }` of turndown: `matches` picks the
elements, and `replacement` gives their Markdown from the Markdown of their
content. Custom rules are tried in order, before the built-in ones. This rule
keeps images as HTML:

```haskell
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import Text.Turndown
import Text.Turndown.Dom (Element (..), outerHtml)

keepImages :: Rule
keepImages = Rule
  { matches = \_ e -> T.toLower e.name == "img"
  , replacement = \_ e _ -> outerHtml e
  }

-- turndown defaultOptions { rules = [keepImages] } "<p>Logo: <img src=\"logo.png\" alt=\"Logo\"></p>"
-- "Logo: <img src=\"logo.png\" alt=\"Logo\">"
```

The `RuleContext` holds what the rules of turndown read from the DOM: the
options, the parent element, the previous and next siblings, the index of the
element among the element children of its parent, and whether it is inside a
`<code>` element.

## Modules

- `Text.Turndown`: `turndown`, `Options`, `defaultOptions`, `Rule`,
  `RuleContext` and `escapeMarkdown`.
- `Text.Turndown.Dom`: the DOM that rules see (`Node`, `Element`,
  `textContent`, `attribute`), HTML parsing, and serialization (`outerHtml`).
- `Text.Turndown.Whitespace`: white space as JavaScript defines it
  (`isJsSpace`, `jsTrim`), for custom rules that must match turndown.

## Differences from turndown

These parts of turndown are not ported:

- `linkStyle: 'referenced'` and `linkReferenceStyle`: links are always inline.
- `preformattedCode`.
- `keep` and `remove`: write a custom `Rule` instead.
- `blankReplacement`, `keepReplacement` and `defaultReplacement`: the defaults
  are built in, and a custom `Rule` can replace them for the elements it
  matches.

Elements nested more than 512 deep are moved next to each other at that depth,
as the HTML parser of Chromium does. This keeps the output the same as in the
browser and bounds the work on deeply nested input.

## Development

From the root of the repository:

```bash
stack test turndown
```

The golden tests live in `test/golden/`:

- `inputs/<name>.html`: the HTML inputs.
- `<options>/<name>.md`: what turndown itself makes of each input, with the
  options in `<options>/options.json`. `default` covers every input; the other
  folders cover the inputs their options change.

To write the expected outputs again (after adding an input, or for another
version of turndown), run `scripts/golden.mjs` from a folder whose
`node_modules` has `turndown` and `playwright` (or `@playwright/test`):

```bash
node <path to this package>/scripts/golden.mjs
```

To cover another input with a non-default option set, add an empty
`<name>.md` to that folder first.

## License

MIT, see [LICENSE](LICENSE). turndown is (c) 2017 Dom Christie, MIT. Its white
space collapsing is adapted from collapse-whitespace, (c) 2014 Luc Thevenard,
MIT.
