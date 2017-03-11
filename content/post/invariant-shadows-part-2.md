+++
date = "2015-06-22"
title = "Invariant Shadows - Part 2: Monoidals"
slug = "invariant-shadows-part-2"
categories = [ "articles" ]
tags = [ "scala", "scodec", "fp" ]
+++

In the [last post](/blog/2015/06/18/invariant-shadows/), we built type classes for a subset of invariant functors -- namely, type classes that drew inspiration from the covariant `FlatMap` and `Monad`. In this article, we'll explore invariant shadows of the type classes supporting applicative functors.

Disclaimer: I am not a category theorist, and in both the previous article and this article, I use fairly loose reasoning to explore these type classes. Corrections are welcome.

First, let's review the definition of an applicative functor, using a type class hierachy similar to the one used in [Cats](https://github.com/non/cats). (These examples are simplified versions of the definitions in the Cats codebase.)

```scala
trait Apply[F[_]] extends Functor[F] {
  def ap[A, B](fa: F[A])(ff: F[A => B]): F[B]
}

trait Applicative[F[_]] extends Apply[F] {
  def pure[A](a: A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))
}
```

Along with laws:

```scala
def applyComposition[F[_], A, B, C](fa: F[A], fab: F[A => B], fbc: F[B => C])(implicit F: Apply[F]): Boolean = {
  val compose: (B => C) => (A => B) => (A => C) = _.compose
  F.ap(F.ap(fa)(fab))(fbc) == F.ap(fa)(F.ap(fab)(F.map(fbc)(compose)))
}

def applicativeIdentity[F[_], A](fa: F[A])(implicit F: Applicative[F]): Boolean =
  F.ap(fa)(F.pure((a: A) => a)) == fa

def applicativeHomomorphism[F[_], A, B](a: A, f: A => B)(implicit F: Applicative[F]): Boolean =
  F.ap(F.pure(a))(F.pure(f)) == F.pure(f(a))

def applicativeInterchange[F[_], A, B](a: A, ff: F[A => B])(implicit F: Applicative[F]): Boolean =
  F.ap(F.pure(a))(ff) == F.ap(ff)(F.pure((f: A => B) => f(a)))

def applicativeMap[F[_], A, B](fa: F[A], f: A => B)(implicit F: Applicative[F]): Boolean =
  F.map(fa)(f) == F.ap(fa)(F.pure(f))
```

### Applicative[Codec]

Can we define an `Applicative[Codec]` instance?  We've previously seen how `Codec` has an invariant functor instance but not a covariant functor instance. As such, we can rule out an `Applicative[Codec]`, for if we could define such an instance, it would give rise to a `Functor[Codec]`. Regardless, let's try to implement such an instance, as it is illustrative.

In the last article, it was implied that we can implement pure for `Codec`. Here's an implementation:

```scala
def pure[A](a: A): Codec[A] = new Codec[A] {
  def sizeBound = SizeBound.exact(0)
  def encode(a: A) = Attempt.successful(BitVector.empty)
  def decode(b: BitVector) = Attempt.successful(DecodeResult(a, b))
}
```

The encode operation always returns an empty vector, while the decode operation always returns the full input as the remainder and returns the pure value as the decoded value.

How about `ap`?

```scala
def ap[A, B](ca: Codec[A])(cf: Codec[A => B]): Codec[B] = new Codec[B] {
  def sizeBound = SizeBound.unknown
  def decode(b: BitVector) = (for {
    decA <- DecodingContext(ca)
    defF <- DecodingContext(cb)
  } yield decF(decA)).decode(b)
  def encode(b: B) = {
    ???
  }
}
```

The decode case is easy enough to define using the `DecodingContext` type from scodec, which is equivalent to a `StateT[Attempt, BitVector, ?]` monad transformer stack, threading the remainder of each decode as the state value. We encounter an impasse in `encode` though. We could try to add a `B => A` function, though it is not obvious as to how that would help. Instead, we can use an alternative form of applicative functors known as monoidal functors.

## Monoidal Functors

[(Lax) monoidal functors](https://wiki.haskell.org/Typeclassopedia#Alternative_formulation) are an alternative form of applicative functors -- similar to how monads can be represented with either `pure` and `flatMap` or `unit`, `flatten` (aka `join`), and `map`.

```scala
trait Monoidal[F[_]] extends Functor[F] {
  def unit: F[Unit]
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}
```

Instead of defining `pure` and `ap`, monoidal functors are defined in terms of `unit`, `zip`, and `map`. Note that in the standard/typical definition, `map` is derived from `pure` and `ap` but in the monoidal definition, `map` is left abstract.

The laws for monoidal functors are the following:

```scala
trait Iso[A, B] { def apply(a: A): B }

implicit class IsoOps[A](val a: A) {
  def ~=[B](b: B)(implicit iso: Iso[A, B]): Boolean = iso(a) == b
}

def monoidalLeftIdentity[F[_], A](fa: F[A])(implicit F: Monoidal[F], iso: Iso[F[(Unit, A)], F[A]]): Boolean =
  F.zip(F.unit, fa) ~= fa

def monoidalRightIdentity[F[_], A](fa: F[A])(implicit F: Monoidal[F], iso: Iso[F[(A, Unit)], F[A]]): Boolean =
  F.zip(fa, F.unit) ~= fa

def monoidalAssociativity[F[_], A, B, C](fa: F[A], fb: F[B], fc: F[C])(implicit F: Monoidal[F], iso: Iso[F[(A, (B, C))], F[((A, B), C)]]): Boolean =
  F.zip(fa, F.zip(fb, fc)) ~= F.zip(F.zip(fa, fb), fc)
```

The identity laws state that zipping a value with the unit value yields the original value, ignoring the tuple structure. The associativity law requires `zip` to be associative, again ignoring the tuple structure. Here, like in the Typeclassopedia definition, we defer the equality checking to an isomorphism that ignores the unwanted structure.

We could further generalize this type class by removing the requirement for the `unit` and `map` operations, leaving simply `zip` along with the associativity law -- this is exactly what Scalaz has done with the `scalaz.Zip` type class.

### Converting applicative forms

We can convert any applicative functor to a monoidal functor and vice-versa:

```scala
implicit def applicativeToMonoidal[F[_]](implicit F: Applicative[F]): Monoidal[F] = new Monoidal[F] {
  def unit: F[Unit] = F.pure(())
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] = F.ap(fb)(F.map(fa)(a => (b: B) => (a, b)))
  def map[A, B](fa: F[A])(f: A => B): F[B] = F.map(fa)(f)
}

implicit def monoidalToApplicative[F[_]](implicit F: Monoidal[F]): Applicative[F] = new Applicative[F] {
  def pure[A](a: A): F[A] = F.map(F.unit)(_ => a)
  def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] = F.map(F.zip(fa, ff)) { case (a, f) => f(a) }
}
```

## Monoidal[Codec]

Let's try to implement a `Monoidal` instance for `Codec`.

The definition of `unit` looks very similar to the definition of `pure`:

```scala
def unit: Codec[Unit] = new Codec[Unit] {
  def sizeBound = SizeBound.exact(0)
  def encode(a: A) = Attempt.successful(BitVector.empty)
  def decode(b: BitVector) = Attempt.successful(DecodeResult((), b))
}
```

Now let's try to implement `zip` -- note that we should not be able to, as we know `Codec` doesn't have a covariant functor.

```scala
def zip[A, B](ca: Codec[A], cb: Codec[B]): Codec[(A, B)] = new Codec[(A, B)] {
  def sizeBound = ca.sizeBound + cb.sizeBound
  def decode(b: BitVector) = (for {
    a <- DecodingContext(ca)
    b <- DecodingContext(cb)
  } yield (a, b)).decode(b)
  def encode(ab: (A, B)) = for {
    encA <- ca.encode(ab._1)
    encB <- cb.encode(ab._2)
  } yield encA ++ encB
}
```

Wait, so what are we missing? We have an implementation of `unit` and `zip` and yet we know we can't have a lawful `Monoidal[Codec]` instance due to the fact that there's no covariant functor for `Codec`. Monoidal functors have *three* abstract operations though, not two (like `Applicative`) -- `unit`, `zip`, and `map`. So in order to have a monoidal functor for `Codec`, we need to define `map`, which we know we can't do!

However, we do have `xmap`. What happens if we pair `xmap` with `unit` and `zip`?

## Invariant monoidal functors

Let's define a new invariant shadow of `Monoidal` that extends `InvariantFunctor` instead of `Functor`:

```scala
trait InvariantMonoidal[F[_]] extends InvariantFunctor[F] {
  def unit: F[Unit]
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}
```

We can also port the laws from lax monoidal functors, which are identical except for requiring an `InvariantMonoidal` instance instead of a `Monoidal` instance:

```scala
def xmonoidalLeftIdentity[F[_], A](fa: F[A])(implicit F: InvariantMonoidal[F], iso: Iso[F[(Unit, A)], F[A]]): Boolean =
  F.zip(F.unit, fa) ~= fa

def xmonoidalRightIdentity[F[_], A](fa: F[A])(implicit F: InvariantMonoidal[F], iso: Iso[F[(A, Unit)], F[A]]): Boolean =
  F.zip(fa, F.unit) ~= fa

def xmonoidalAssociativity[F[_], A, B, C](fa: F[A], fb: F[B], fc: F[C])(implicit F: InvariantMonoidal[F], iso: Iso[F[(A, (B, C))], F[((A, B), C)]]): Boolean =
  F.zip(fa, F.zip(fb, fc)) ~= F.zip(F.zip(fa, fb), fc)
```

### Deriving an `InvariantMonoidal` from an `InvariantMonad`

In the same way that a monad yields two monoidal functors (or two applicative functors) -- one that evaluates `F[A]` first and `F[B]` second, and another which evaluates in the reverse order -- an invariant monad gives rise to two invariant monoidal functors. For example, the following implementation evaluates the `F[A]` first and the `F[B]` second:

```scala
implicit def xmonadToXmonoidal[F[_]](implicit F: InvariantMonad[F]): InvariantMonoidal[F] = new InvariantMonoidal[F] {
  def unit: F[Unit] = F.pure(())
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    F.xflatMap(fa)(a => F.xmap(fb)(b => (a, b))(ab => ab._2))(ab => ab._1)
  def xmap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = F.xmap(fa)(f)(g)
}
```

### Sharing unit/zip

Because the invariant monoidal differs from the covariant monoidal only in the defintion of `xmap`/`map`, we could extract a type class that captures the signature of `unit` and `zip` and the laws that govern their interaction.

```scala
trait MonoidalBase[F[_]] {
  def unit: F[Unit]
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}
trait Monoidal[F[_]] extends MonoidalBase[F] with Functor[F]
trait InvariantMonoidal[F[_]] extends MonoidalBase[F] with InvariantFunctor[F]
```

## Applicability to Codec

In the last article, we saw that `Codec` supported both `flatZip` and `consume`, and how those methods exist due to the invariant monad structure of `Codec`. In fact, those methods both existed *before* the invariant monad structure was extracted. Is the same true for `unit` and `zip`? That is, do those methods exist under some other name, their existence justified by their usefulness in binary codec creation, rather than satisfying a type class definition?

It turns out that both of these operations do exist already. The `unit` operation is provided by `scodec.codecs.ignore(0L)`, which returns a `Codec[Unit]` that encodes an empty bit vector. The `zip` operation is provided by the `pairedWith` method on codec -- which has the operator alias `~`.

We did not provide an invariant shadow of applicative functors, though that is worth exploring, perhaps in a future article.

