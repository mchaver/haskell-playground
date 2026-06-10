# haskell-playground

This is a space to explore the Haskell programming language and ecosystem. It is a collection of small projects to test different language extensions, libraries and domains.

## Projects

- [`matching-engine`](matching-engine/) — a correctness-first central limit order book and matching engine, with a Hedgehog property suite. See its [README](matching-engine/README.md) for build/test/lint/format tasks.
- [`effect-example`](effect-example/) — a small [Effectful](https://hackage.haskell.org/package/effectful) demo: scoped, dynamically-dispatched effects with real-IO and pure interpreters.

## Shared tooling

Lint and formatting rules are defined once at the repo root and shared by every
project — both tools find them by searching up from each source file:

- **[`.hlint.yaml`](.hlint.yaml)** — [HLint](https://github.com/ndmitchell/hlint) house rules: no partial functions, no `error`/`undefined`, no lazy/quadratic folds, and no lazy container/monad modules (use the `.Strict` variants).
- **[`fourmolu.yaml`](fourmolu.yaml)** — [Fourmolu](https://github.com/fourmolu/fourmolu) formatting (2-space indent, leading commas, `-- |` Haddock).

```
hlint .          # lint every project from the repo root
fourmolu --mode check $(find . -name '*.hs' -not -path '*/dist-newstyle/*')
```

Build artifacts are ignored repo-wide via the root [`.gitignore`](.gitignore).
