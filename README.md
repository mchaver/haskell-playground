# haskell-playground

This is a space to explore the Haskell programming language and ecosystem. It 
is a collection of small projects to test different language extensions, 
libraries and domains.

## Projects

- [`matching-engine`](matching-engine/): a limit order book and matching engine.
- [`itch-codec`](itch-codec/): an encoder/decoder for a subset of the NASDAQ TotalView-ITCH 5.0 binary market-data protocol. 
- [`effect-example`](effect-example/): a small [Effectful](https://hackage.haskell.org/package/effectful) demo.

## Shared tooling

Lint and formatting rules are defined in the repo root and shared by every
project:

- **[`.hlint.yaml`](.hlint.yaml)**: [HLint](https://github.com/ndmitchell/hlint) house rules: no partial functions, no `error`/`undefined`, no lazy/quadratic folds, and no lazy container/monad modules (use the `.Strict` variants).
- **[`fourmolu.yaml`](fourmolu.yaml)**: [Fourmolu](https://github.com/fourmolu/fourmolu) formatting (2-space indent, leading commas, `-- |` Haddock).
