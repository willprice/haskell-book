# Type classes

Here we take a look at some common type classes, starting with the definition of
a bunch of properties important in type class laws.

Contents:

* [Binary operator properties](#Binary-operator-properties)
  * [Associativity](#Associativity)
  * [Commutativity](#Commutativity)
  * [Distributivity](#Distributivity)
  * [Identity element](#Identity-element)
  * [Inverse element](#Inverse-element)
* [Type classes](#Type-classes)
  * [Semigroup](#Semigroup)
  * [Monoid](#Monoid)
  * [Functor](#Functor)
  * [Applicative](#Applicative)


## Binary operator properties

A binary operator `￮` and a *set* of elements `A` where `a`, `b`, are `c`
are elements of `A` can have a bunch of properties like *associativity*,
*distributivity*, *commutativity*. These are detailed below.


## Associativity

Definition: `(a ￮ b) ￮ c = a ￮ (b ￮ c)`

Examples:

- `(+, R)`: `(1 + 2) + 3 = 6 = 1 + (2 + 3) = 6`
- `(++, [a])`: `([1, 2] ++ [3, 4]) ++ [5, 6] = [1, 2] ++ ([3, 4] ++ [5, 6])`

Non-examples:

- `(/, R)` since `(1 / 2) / 2 ≠ 1 / (2 / 2)`
- `(-, R)` since `(1 - 2) - 3 = -4 ≠ 1 - (2 - 3) = 2`


## Commutativity

Definition: `a ￮ b = b ￮ a`

Examples:

- `(+, R)`: `3 + 2 = 2 + 3`
- `(*, R)`: `3 * 2 = 2 * 3`

Non-examples:

- `(-, N)` since `0 - 1 = -1` which is not in `N`.
- `(++, [a])` since `[1] + [2] ≠ [2] + [1]`
- `(/, R)` since `1/2 ≠ 2/1`


## Distributivity

Given two binary operators `(￮, ▫)` and a *set* `S`.

Definition:  `a ￮ (b ▫ c) = (a ▫ b) ￮ (a ▫ c)`

Examples:

- `((*, +), R)`: `2 * (1 + 3) = 8 = (2 * 1) + (2 * 3)`

Non-examples:

- `((+, *), R)`: `2 + (1 * 3) = 5 ≠ (2 + 1) * (2 + 3) = 15` (order of binary
  operators matter!)
- `((/, *), R): 2 / (1 * 3) = 2 / 3 ≠ (2 / 1) * (2 / 3) = 4 / 3`
- `((*, /), R): 2 * (1 / 3) = 2 / 3 ≠ (2 * 1) / (2 * 3) = 1 / 3`

## Identity element

A binary operator `￮` has a identity element `e` if for all `a` in `A` then `e ￮ a =
a` (and usually `a ￮ e = a`) forming the tuple `(￮, e, A)`

Examples:

- `(+, 0, R)`: `2 + 0 = 2 = 0 + 2`
- `(*, 1, R)`: `2 * 1 = 2 = 1 * 2`
- `(++, [], [a])`: `[1, 2] ++ [] = [1, 2] = [] ++ [1, 2]`

Non-examples: take the top 2 examples from above and pick any other real number
instead of those listed.


## Inverse value

A binary operator `￮` has an inverse element `a^-1` if for all `a` in `A` then `a ￮ a^-1 =
e` where `e` is the identity element forming the tuple `(￮, e, A)`

Examples:

- `(*, 1, R)`: `2 * 0.5 = 1`
- `(+, 0, R)`: `1 + (-1) = 0`


Non-examples:

- `(*, 1, N)` since `1` has no inverse in the naturals.


## Semigroup

A binary operator `￮` and a set `A` form a semigroup: `(￮, A)` if...

* `￮` is [associative](#Associativity)

Examples:

- `(+, R)`: `(1 + 2) + 3 = 1 + (2 + 3)`
- `(++, [a])`: `[1] ++ ([2] ++ [3]) = [1, 2, 3] = ([1] ++ [2]) ++ [3]`

Non-examples:

- `(/, R)`: `1 / (2 / 3) = 1.5 ≠ (1 / 2) / 3 = 1 / 6`


Haskell type class:

```haskell
class Semigroup (a :: *) where
  (<>) :: a -> a -> a
```


## Monoid

A binary operator `￮` and a set `A` form a Monoid: `(￮, A)` if...

* `￮` is [associative](#Associativity)
* `￮` has an [identity element](#Identity-element) (`mempty`)


Haskell type class:

```haskell
class Semigroup a => Monoid (a :: *) where
  mempty :: a
```


## Functor

A [mathematical functor](https://en.wikipedia.org/wiki/Functor) is a 'map
between categories', in programming language speak this can be more plainly
thought of as a way of applying functions over structure (like applying a
function to every item in a list, or tree, or graph etc)


Laws:

- Identity: `fmap id = id`
- Composition: `fmap (f . g) = (fmap f) . (fmap g)`

Haskell type class:

```haskell
class Functor a where
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  (<$>) = fmap
```


## Applicative

Laws:

- Identity: `pure id <*> v = v`
- Homomorphism: `pure f <*> pure x = pure (f x)`
- Interchange: `u <*> pure y = pure ($ y) <*> u`
- Composition: `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`

Haskell type class:

```haskell
class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```
