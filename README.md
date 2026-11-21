# haskell-package

A monorepo of the various packages I create for the Haskell ecosystem. Each package lives in its own folder under `package/`, with its own README, and all of them are MIT licensed.

## Packages

| Package | Folder | Description |
| --- | --- | --- |
| `r7-asyncapi` | [`package/r7-asyncapi`](package/r7-asyncapi) | AsyncAPI 3.0 documents with [openapi3](https://hackage.haskell.org/package/openapi3) schemas, and messages derived from Haskell types. |
| `r7-turndown-hs` | [`package/r7-turndown-hs`](package/r7-turndown-hs) | HTML to Markdown, a port of the [turndown](https://github.com/mixmark-io/turndown) JavaScript library with exactly its output. |

## Build and test

The packages build with [Stack](https://docs.haskellstack.org/):

```bash
stack build
```

```bash
stack test
```

To build or test one package, name it, for example `stack test r7-turndown-hs`.

## License

MIT, see [LICENSE](LICENSE). Each package has its own `LICENSE` with the copyright of the code it ports.
