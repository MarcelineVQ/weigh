This is a modified copy of Chris Done's neat library Weigh, it has been modified to track more stats than the original, increasing its utility.

---

Measures the memory usage of a Haskell value or function

## Example use

``` haskell
import Weigh

-- | Weigh integers.
main :: IO ()
main =
  mainWith (do integers
               ints
               maxRes)

-- | Just counting integers.
integers :: Weigh ()
integers =
  do func "integers count 0" count 0
     func "integers count 1" count 1
     func "integers count 2" count 2
     func "integers count 3" count 3
     func "integers count 10" count 10
     func "integers count 100" count 100
  where count :: Integer -> ()
        count 0 = ()
        count a = count (a - 1)

-- | We count ints and ensure that the allocations are optimized away
-- to only two 64-bit Ints (16 bytes).
ints :: Weigh ()
ints =
  do validateFunc "ints count 1" count 1 (maxAllocs 24)
     validateFunc "ints count 10" count 10 (maxAllocs 24)
     validateFunc "ints count 1000000" count 1000000 (maxAllocs 24)
  where count :: Int -> ()
        count 0 = ()
        count a = count (a - 1)

-- | Comparing residency between a strict fold and a lazy one.
-- Lazy should fail the limit.
maxRes :: Weigh ()
maxRes =
  do validateFunc "strict fold" (lfold' (+) 0) list $ maxResidency 120 -- MB
     validateFunc "lazy fold" (lfold (+) 0) list $ shouldFail (maxResidency 120)
  where
    list = [1..1000000 :: Int]
    lfold _ z [] = z; lfold f z (x:xs) =
      lfold f (f z x) xs
    lfold' _ a [] = a; lfold' f a (x:xs) =
      let a' = f a x in a' `seq` lfold' f a' xs
```

Output results:

```
Case                     Bytes  Res(MB)  GCs  Check
integers count 0             0       <1    0  OK
integers count 1            32       <1    0  OK
integers count 2            64       <1    0  OK
integers count 3            96       <1    0  OK
integers count 10          320       <1    0  OK
integers count 100       3,200       <1    0  OK
ints count 1                 0       <1    0  OK
ints count 10                0       <1    0  OK
ints count 1000000           0       <1    0  OK
strict fold         95,999,920       92  186  OK
lazy fold          169,259,440      165  322  OK
```

You can try this out with `stack test` in the `weight` directory.

To try out other examples, try:

* `stack test :weigh-maps --flag weigh:weigh-maps`
