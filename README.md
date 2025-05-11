lIsts of action libraries :

- [raaz](https://hackage.haskell.org/package/raaz-0.0.1/docs/Raaz-Core-MonoidalAction.html)
- [monoid-extra](https://github.com/diagrams/monoid-extras)
- [acts](https://hackage.haskell.org/package/acts)
- [semigroup-actions](https://hackage.haskell.org/package/semigroups-actions)

# semigroup-extras


This library initially comes from forking the [monoid-extra](https://github.com/diagrams/monoid-extras) library from the **diagrams** software teams.

## Features

- semigroup actions (left and right)
- semidirect product

## Comparison with `monoid-extras`

### What is in `monoid-extras` and not in this library ?

`monoid-extras` has way more to offer than this library. Most of the features of `monoid-extras` are not implemented here. If you need those features, you should use `monoid-extras` instead.

### What is in this library and is not in `monoid-extras` ?

Only the two following :

- Right actions (`monoid-extras` only implements left actions)
- Freedom in action instances (see **Why writing another library instead of a fork ?**)

#### Why writing another library instead of a fork ?

In `monoid-extras`, instances of the class `Action` are driven by the first parameter, that is instances of the form `Action s SomeType` are likely to overlap with the `monoid-extras` instances, namely the following instance :

``` Haskell
instance Action m s => Action (Maybe m) s where
  act Nothing  s = s
  act (Just m) s = act m s
```

This library choses another approach which is to never write any `LAction` or `RAction` instance containing  a type variables, that is, within the library, there is no instance of the form `LAction s SomeType` nor actions of the type `LAction SomeSemigroup t`.

This way, it is up to the user of the library to decide whether they want `LAction` (or `RAction`) instances to be driven by their first of second parameter. Concretely, you can decide whether you'd rather have the `Maybe` instance mentioned above or if you'd rather have instances like

``` Haskell
instance LAction s x => Action s [x] where
 lact s  = map (lact s)
```

Note that, even though it is very tempting to right a functor instance as shown below to generalize the above instance, it will overlap with almost every instances one could come up with. Namely, this is always bad :

``` Haskell
instance (Functor f, LAction s x) => Action s (f x) where
   lact s = fmap (lact s)
```

Since this design choice is not compatible with a lot of  `monoid-extras`, this library was born. However, `monoid-extras`is much more mature than this library. So unless you need the freedom mentionned above, you should probably use `monoid-extras` instead of this library.
