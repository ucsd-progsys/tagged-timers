# README

`tagged-timers` is a simple package for timing different `IO`
that occur within a program and grouping the results
according to dynamically generated `String` tags.

## Example

For a simple example of usage, see `examples/Example.hs`

```haskell
main :: IO ()
main = do
  t <- T.create
  T.time t "cat" (act "cat" 5)
  T.time t "dog" (act "dog" 2)
  r <- T.result t
  putStrLn $ "Time Result: " ++ show r
```

which, when executed, yields the following behavior:

```
Prelude> :l examples/Example.hs
...
*Main> main
Starting action cat
Oh so sleepy...
(5 seconds later)
Finished action cat
Starting action dog
Oh so sleepy...
(2 seconds later)
Finished action dog
Time Result: [("cat",5.003739s),("dog",2.003825s)]
```
