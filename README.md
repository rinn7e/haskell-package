# haskell-package

A monorepo of the various packages I create for the Haskell ecosystem. Each package lives in its own folder under `package/`, with its own README, and all of them are MIT licensed.

## Packages

| Package | Folder | Description |
| --- | --- | --- |
| `turndown` | [`package/turndown-hs`](package/turndown-hs) | HTML to Markdown, a port of the [turndown](https://github.com/mixmark-io/turndown) JavaScript library with exactly its output. |

## Build and test

The packages build with [Stack](https://docs.haskellstack.org/):

```bash
stack build
```

```bash
stack test
```

To build or test one package, name it, for example `stack test turndown`.

## License

MIT, see [LICENSE](LICENSE). Each package has its own `LICENSE` with the copyright of the code it ports.
