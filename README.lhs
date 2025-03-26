# memo-map

Utility for memoizing an effectful function (e.g. database query) using a map.

## Example

<!--
```haskell
module Main (main) where

import Prelude
import Text.Markdown.Unlit ()
```
-->

```haskell
import MemoMap
```

For demonstration, a silly little function that performs an effect and returns a value:

```haskell
f :: Int -> IO Int
f x = do
  putStrLn ("f " <> show x)
  pure (x * 10)
```

If `f` may be applied to the same argument repeatedly, perhaps it would be advantageous
to save the results from a previous application rather than repeating the action.

<!--
```haskell
main :: IO ()
main = do
```
-->

### Create a MemoMap

Turn `f` into a `MemoMap`.

```haskell
  mm <- newMemoMap f
```

### Use the memoized function

Use `runMemoMap mm` instead of `f` to run a _memoized_ version of `f`.

```haskell
  print =<< runMemoMap mm 2
  print =<< runMemoMap mm 2
  print =<< runMemoMap mm 3
```

Output:

    f 2
    20
    20
    f 3
    30

### Inspect the cache

You can also use `getMemoMap` to inspect the stored values.

```haskell
  print =<< getMemoMap mm
```

Output:

    fromList [(2,20),(3,30)]

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
