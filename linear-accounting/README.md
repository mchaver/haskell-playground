# linear-accounting

A small exploration of GHC's `LinearTypes` extension and the
[`linear-base`](https://hackage.haskell.org/package/linear-base) library,
built around the idea: money obeys a conservation law, and linear types can
make that law a compile-time guarantee.

```
make test
make run
make check   # build + test + lint + format-check
```

## The idea: linearity = conservation

A value of a linear type must be used exactly once. It cannot be
duplicated and it cannot be dropped. Map that onto money and you get the two
properties an accounting system most wants:

| Linear-type rule | Accounting meaning |
|---|---|
| can't be duplicated | no counterfeiting. one pile can't become two |
| can't be dropped | no leaks. funds can't silently vanish |

So `Money` (`src/Accounting/Money.hs`) is a linear resource. Its data
constructor is hidden, and it deliberately has no `Consumable` and no
`Dupable` instance.

```haskell
data Money -- constructor hidden; linear

split :: Amount -> Money %1 -> Either Money (Money, Money)
merge :: Money %1 -> Money %1 -> Money
```

The `%1 ->` arrow is the whole story: a function with a `%1 ->` argument
promises to consume that argument exactly once. `split` and `merge` only ever
move value around, so the total is conserved by construction.

## The trusted boundary

Value still has to enter and leave the system somewhere. Only two operations 
can change how much money exists:

```haskell
mint :: Amount    -> Money       -- funds enter (deposit, mint, opening float)
burn :: Money %1  -> Ur Amount   -- funds leave (withdrawal, payout)
```

`mint` conjures a linear `Money` from a plain number; `burn` consumes a `Money`
and hands back its magnitude as an `Ur Amount` — an *unrestricted* value you
can freely copy and print. Everything else is built on top and conserves value.

## Accounts as scoped resources

`Account` (`src/Accounting/Ledger.hs`) owns a `Money` balance, so an account is
*itself* linear. This brings in the other thing linear types are good at
resource safety. `withAccount` brackets an account's lifetime:

```haskell
withAccount :: AccountId -> Amount -> (Account %1 -> Account) %1 -> Ur Amount
```

The continuation is handed a linear `Account` and must return one an `Account`. It
cannot leak it or drop it. "Forgot to settle the account" is a type error,
not a runtime bug. `transfer` moves value between two accounts and reports
whether the source could cover it, always conserving the grand total:

```haskell
transfer :: Amount -> Account %1 -> Account %1 -> (Account, Account, Ur Bool)
```

## What the compiler rejects

`app/Main.hs` ends with a gallery of mistakes kept as comments. Uncomment any
one and the package stops compiling. All three are the same underlying error —
a *multiplicity* mismatch, `One` vs `Many`:

```haskell
-- balance would vanish: 'acct' used 0 times
badDrop :: Account %1 -> Account
badDrop acct = openAccount 99 0

-- balance would double: 'acct' used twice (Money has no Dupable)
badDup :: Account %1 -> (Account, Account)
badDup acct = (acct, acct)
```

```
error: [GHC-18872]
    • Couldn't match type 'Many' with 'One'
        arising from multiplicity of 'acct'
```

## The properties (`test/Main.hs`)

Linearity already makes "can't duplicate or drop" a compile-time fact, so the
runtime tests don't re-prove that. They pin down the arithmetic linearity lets
us trust:

| Property | What it guarantees |
|---|---|
| split conserves | the two pieces sum back to the input pile |
| split succeeds iff funded | `split n` returns `Right` exactly when the pile covers `n` |
| merge conserves | merging two piles adds their magnitudes, nothing lost or invented |
| transfer conserves | the grand total across both accounts is unchanged |
| transfer moves n on success | on success the source is debited `n` and the destination credited `n`; on failure both are untouched |

## Limits

The conservation guarantee holds at the boundary of this API. Linearity
disciplines everything built on `mint`/`burn`, but `mint` itself is a trusted
operation that creates value from a plain number, there has to be one, and it
is where you'd audit for inflation. At the top level (e.g. in `main`) values
handed back out of linear functions are ordinary unrestricted Haskell values;
the "use exactly once" obligation bites inside the `%1 ->` typed regions and
continuations, which is exactly where the accounting logic lives.

## Developing

| Command | What it does |
|---|---|
| `make build` | Compile library, demo, and tests. |
| `make test` | Run the conservation property suite. |
| `make run` | Run the demo. |
| `make lint` | HLint over `src app test`. |
| `make format`, `make format-check` | Fourmolu (rewrite, verify). |
| `make check` | `build` + `test` + `lint` + `format-check`. |

Lint and formatting rules are shared across the repo and live one level up:
[`../.hlint.yaml`](../.hlint.yaml) and [`../fourmolu.yaml`](../fourmolu.yaml).

## References

- [Linear types in GHC](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/linear_types.html) (user's guide)
- [`linear-base`](https://hackage.haskell.org/package/linear-base): `Ur`, `move`, `Consumable`, `Dupable`
- [Linear Haskell (POPL 2018)](https://arxiv.org/abs/1710.09756): the original paper
