# untyped

The pure untyped lambda calculus.

## Examples

* `ex1.u` - Unbound top-level variable
* `ex2.u` - Top-level lambda abstraction
* `ex3.u` - Simple application
* `ex4.u`, `ex5.u` - Church Boolean test combinator where

```
true  = lambda t. lambda f. t
false = lambda t. lambda f. f
test  = lambda l. lambda m. lambda n. l m n
```

* `ex6.u`, `ex7.u` - Pair combinators with Church Booleans where

```
pair   = lambda f. lambda s. lambda b. b f s
first  = lambda p. p true
second = lambda p. p false
```

* `ex8.u`, `ex9.u`, `ex10.u` - Church Numerals where

```
0 = lambda s. lambda z. z
1 = lambda s. lambda z. s z
2 = lambda s. lambda z. s (s z)
3 = lambda s. lambda z. s (s (s z))
etc.
```

and

```
succ = lambda n. lambda s. lambda z. s (n s z)
```
