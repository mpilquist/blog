+++
date = "2015-06-18"
title = "Invariant Shadows"
categories = [ "articles" ]
tags = [ "scala", "scodec", "fp" ]
+++

A common use case when working with binary protocols is decoding a value and then using the decoded value to determine how to decode the remaining bits. For example, consider a simple framing structure, made up of a 2-byte big endian unsigned integer field followed by `n` bytes, where `n` is the value in the first field. In order to decode this structure, we need to first decode the 2-byte size field, then use the decoded size to take that number of bytes from the remainder.

This can be represented with [scodec](http://scodec.org) like this:

```scala
import scodec._
import scodec.bits._
import scodec.codecs._

val decoder: Decoder[ByteVector] = uint16 flatMap { size => bytes(size * 8L) }
```

Here, we used `flatMap` to express the dependency between `uint16: Codec[Int]` and the bytes decoder. However, `flatMap` on `Codec` returns
a `Decoder[B]`, not a `Codec[B]`, so by using `flatMap`, we lose the ability to encode.

It turns out that it is impossible to define `flatMap` for `Codec` with the expected signature. Consider this attempt:

```scala
trait Codec[A] extends Encoder[A] with Decoder[A] { self =>

  def flatMap[B](f: A => Codec[B]): Codec[B] = new Codec[B] {
    def sizeBound = self.sizeBound.atLeast
    def decode(b: BitVector): Attempt[DecodeResult[B]] =
      self.decode(b).flatMap { res => f(res.value).decode(res.remainder) }
    def encode(b: B): Attempt[BitVector] =
      ???
  }
}
```

The implementation of `decode` is straightforward, but we arrive at an impasse when trying to implement `encode`. We have a `Codec[A]`, a function `A => Codec[B]` and a value of type `B`. We cannot use the `Codec[A]` to encode unless we have an `A`, and we cannot get access to a `Codec[B]` unless we have an `A` to apply to the function. Hence, we cannot implement `flatMap` on `Codec` -- meaning that there is no monad for `Codec`!

So how were we able to call `flatMap` on `Codec` in our first example? That worked because `Decoder` defines a `flatMap` method like this:

```scala
trait Decoder[+A] { self =>

  def flatMap[B](f: A => Decoder[B]): Decoder[B] = new Decoder[B] {
    def decode(b: BitVector): Attempt[DecodeResult[B]] =
      self.decode(b).flatMap { res => f(res.value).decode(res.remainder) }
  }
}
```

Because `Codec` extends from `Decoder`, it inherits this definition of `flatMap`, which is why flat mapping a `Codec` results in a `Decoder`.

Nonetheless, we can change the signature of `flatMap` slightly in order to get much of the utility of `flatMap` without running in to our impasse when encoding. In our ill-fated attempt at implementing `flatMap`, we were short a value of `A`. Hence, we can ask the caller to provide the `A` value. A particularly useful way of doing so is changing the return type from `Codec[B]` to `Codec[(A, B)]`.

This has implications on both decoding and encoding -- when decoding, we need to return the decoded `A` in the overall result instead of using it solely to generate the `Codec[B]`, whereas in encoding, we need to encode the input `A`, apply the function with the same `A` to create a `Codec[B]`, encode the input `B`, and finally concatenate the encoded forms of `A` and `B`. The signature is the same as `flatMap` except the result type is `Codec[(A, B)]` instead of `Codec[B]` -- so for now, let's call this `flatZip` since it zips `A` and `B` in to a tuple.

```scala
trait Codec[A] extends Encoder[A] with Decoder[A] { self =>

  def flatZip[B](f: A => Codec[B]): Codec[(A, B)] = new Codec[(A, B)] {
    def sizeBound = self.sizeBound.atLeast
    def decode(b: BitVector): Attempt[DecodeResult[(A, B)]] =
      self.decode(b).flatMap { res =>
        val a = res.value
        val bres = f(a).decode(res.remainder)
        bres.map { _.map { b => (a, b) } }
      }
    def encode(ab: (A, B)): Attempt[BitVector] = for {
      encA <- self.encode(ab._1)
      codecB = f(ab._1)
      encB <- codecB.encode(ab._2)
    } yield encA ++ encB
  }
}
```

Another solution to the `flatMap` dilemma is keeping the return type as `Codec[B]` but adding another parameter to the method that lets us materialize an `A` from what we have -- namely, a `B`.

```scala
trait Codec[A] extends Encoder[A] with Decoder[A] { self =>

  def consume[B](f: A => Codec[B])(g: B => A): Codec[B] = new Codec[B] {
    def sizeBound = self.sizeBound.atLeast
    def decode(b: BitVector): Attempt[DecodeResult[B]] =
      self.decode(b).flatMap { res => f(res.value).decode(res.remainder) }
    def encode(b: B): Attempt[BitVector] = {
      val a = g(b)
      for {
        encA <- self.encode(a)
        codecB = f(a)
        encB <- codecB.encode(b)
      } yield encA ++ encB
    }
  }
}
```

One interesting use case for `consume` is handling header fields whose values are derived from the body. For instance, see [this example from scodec](https://github.com/scodec/scodec/blob/2d790190617b8025ce22c7c9f041f1a7a52a8d84/shared/src/test/scala/scodec/examples/ProductsExample.scala#L86-L103).

An interesting property of these methods is that they can be implemented in terms of one another, as long as we have the `xmap` operation.

 - `consume(f)(g) == flatZip(f).xmap[B](ab => ab._2)(b => (g(b), b))`
 - `flatZip(f) == consume(a => f(a).xmap[(A, B)](b => (a, b))(ab => ab._2))(ab => ab._1)`

The scodec library provides both of these methods, as they are useful in different scenarios. However, these identities show that they are just different formulations of the same concept.

### A note on naming

The `flatZip` name leaves a lot to be desired. We selected it based on an appeal to how the method is `flatMap`-like in signature but returns a tuple -- zipping the `A` and `B`. `flatMapAndZip` is way too long and a bit deceiving, as it implies that the operation is the result of composing zipping with `flatMap`. Additionally, we are only really zipping on the decoder side -- when encoding, the user is passing the tuple to us. Zip describes the action that builds the tuple, but it would be clearer to describe the tuple itself. Using `product` to refer to the product type `(A, B)`, we could call this operation `flatMapProduct`.

The `consume` name could also be improved upon. For starters, it is evocative. Worse, the rationale for its name applies equally well when using `consume` as an alias for `flatMap`. Re-examining the signature shows us that `consume` is like `flatMap` but takes an extra parameter, `B => A`. There doesn't seem to be much help in naming there. Instead, let's use our identity to help name the operation. We've shown that `consume` can be written in terms of `flatMapProduct` and `xmap`. Hence, `consume` is like an invariant version of the (covariant) `flatMap`. Informed by the fact that we use `xmap` as the invariant form of the (covariant) `map` method, we'll call this operation `xflatMap`.

Knowing that `flatMapProduct` depends on `xmap`, let's use the same convention there as well and call it `xflatMapProduct`.

Naming is hard.

## Generalizing

Let's extract a type class for this operation. Here, we'll pick `xflatMap` as the primary operation and define `xflatMapProduct` in terms of `xflatMap`, but the inverse would be equally useful. Let's also assume we have an invariant functor type class available:

```scala
trait InvariantFunctor[F[_]] {
  def xmap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
}

trait InvariantFlatMap[F[_]] extends InvariantFunctor[F] {

  def xflatMap[A, B](fa: F[A])(f: A => F[B])(g: B => A): F[B]

  def xflatMapProduct[A, B](fa: F[A])(f: A => F[B]): F[(A, B)] =
    xflatMap[A, (A, B)](fa)(a => xmap[B, (A, B)](f(a))(b => (a, b))(ab => ab._2))(ab => ab._1)
}
```

What laws can we write that govern the behavior of `xflatMap`? We could use the same laws as the normal `FlatMap` type class adjusted for the extra `g` parameter -- namely, that `xflatMap` is associative.

```scala
def xflatMapAssociativity[F[_], A, B, C](
  fa: F[A], f: A => F[B], fi: B => A, g: B => F[C], gi: C => B
)(implicit F: InvariantFlatMap[F]): Boolean =
  F.xflatMap(F.xflatMap(fa)(f)(fi))(g)(gi) ==
    F.xflatMap(fa)(a => F.xflatMap(f(a))(g)(gi))(gi andThen fi)
```

### Parallels with `FlatMap`

The (covariant) `FlatMap` type class corresponds to `InvariantFlatMap` in the same way that (covariant) `Functor` corresponds to `InvariantFunctor`. For example, any `FlatMap` instance gives rise to an `InvariantFlatMap` instance which simply ignores the `g` parameter.

What about `xflatMapProduct` though? There's nothing to remove or ignore in its signature, so how can there be a correspondence to a covariant equivalent? The covariant equivalent of `xflatMapProduct` differs not in signature, but in implementation -- specifically, it uses the covariant `map` instead of the invariant `xmap`. In fact, the covariant equivalent of `xflatMapProduct` is known as `mproduct` in Haskell -- short for "monad product", which is interesting because it does not rely on a full monad struture, but rather, only the structure of `FlatMap`/`Bind`. Naming is hard. (To be fair, `mproduct` exists in the Haskell standard library, which does not include the `Bind` type class.)

## Further Generalization with Fast and Loose Reasoning

Building off these correspondences, can we create an `InvariantMonad` type class by adding a `pure` method to an `InvariantFunctor`?

```scala
trait InvariantMonad[F[_]] extends InvariantFlatMap[F] {
  def pure[A](a: A): F[A]
}
```

With the associativity law inherited from `InvariantFlatMap` along with a left and right identity law:

```scala
def invariantMonadLeftIdentity[F[_], A, B](a: A, f: A => F[B], fi: B => A)(implicit F: InvariantMonad[F]): Boolean =
  F.xflatMap(F.pure(a))(f)(fi) == f(a)

def invariantMonadRightIdentity[F[_], A](fa: F[A])(implicit F: InvariantMonad[F]): Boolean =
  F.xflatMap(fa)(a => F.pure(a))(identity) == fa
```

We can now port arbitrary functions that work with monads to work with invariant monads -- compensating for the extra inverse function as appropriate.

## Parting Thoughts

Starting with a concrete example from scodec, we allowed the type system to guide us in the development of `flatZip` and `consume`. By generalizing the resulting operations, we were able to focus on the structure, with no implied meaning from binary serialization. Once we realized that we had build an invariant form of `FlatMap`, we were able to use loose reasoning based on correspondences with familiar type classes to develop an invariant form of `Monad`.

Continuously shifting between working with concrete types and working with minimally expressive type classes, allowing the findings from each style to inform the work in the opposite style, is a powerful technique in API development.

Further work will focus on development of an `InvariantApplicative` type class, along with finding more applications of these invariant type classes in scodec.

## Acknowledgements

Special thanks to Paul Chiusano for some thoughtful discussion of these topics.
