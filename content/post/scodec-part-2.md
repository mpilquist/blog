+++
date = "2013-06-01"
title = "scodec - Part 2 - Conversions"
slug = "scodec-part-2"
categories = [ "articles" ]
tags = [ "scala", "scodec" ]
+++

In [Part 1](/blog/2013/05/27/scodec-intro/), we looked at the codec trait and a way to create a `Codec[(A, B)]` out of a `Codec[A]` and a `Codec[B]`. In this post, we'll look at converting a `Codec[A]` to a `Codec[B]` using a type driven approach and we'll get our feet wet with some category theory.

<!--more-->

Consider this:

``` scala
case class Point2D(x: Int, y: Int)
```

If we have a codec for an integer, we can create a `Codec[(Int, Int)]` easily using the `TupleCodec` from Part 1:

``` scala
val int: Codec[Int] = ...
val tuple: Codec[(Int, Int)] = new TupleCodec(int, int)
```

Working directly with tuples isn't pleasant, so we'd like to be able to create a `Codec[Point2D]` from a `Codec[(Int, Int)]`:
``` scala
val point: Codec[Point2D] = ???
```

Hence, we need a function that can convert a `Codec[A]` to a `Codec[B]`. This looks like functor map, so let's try it:
``` scala
def map[A, B](ca: Codec[A])(f: A => B): Codec[B] = new Codec[B] {
  def encode(b: B): String \/ BitVector = ???
  def decode(buf: BitVector): String \/ (BitVector, B) = ???
}
```

Let's start with the implementation of `decode`. We have:

 - `buf`, a value of `BitVector`
 - `f`, a function `A => B`
 - `ca`, a `Codec[A]`

Using those values, we need to produce a `String \/ (BitVector, B)`. We can use the values as jigsaw puzzle pieces and find a way to put all the proper shapes together. In this case, we can call `decode` on `ca` to get back a value of `String \/ (BitVector, A)`. Then we can map `f` over the inner `A` to get a `String \/ (BitVector, B)`:
``` scala
def map[A, B](ca: Codec[A])(f: A => B): Codec[B] = new Codec[B] {
  def encode(b: B): String \/ BitVector = ???
  def decode(buf: BitVector): String \/ (BitVector, B) =
    ca.decode(buf) map { case (rest, a) => (rest, f(a)) }
}
```

So far so good. Let's try the same type driven approach with the implementation of `encode`. We have:

 - `b`, a value of `B`
 - `f`, a function `A => B`
 - `ca`, a `Codec[A]`

It seems we are at an impasse -- no matter which value we start with, there's no way to combine it with the other values. Intuitively, we know that encode should use `ca.encode` in its implementation, so let's try that:
``` scala
def map[A, B](ca: Codec[A])(f: A => B): Codec[B] = new Codec[B] {
  def encode(b: B): String \/ BitVector =
    ca.encode(???)
  ...
}
```

In order to call encode, we need a value of `A` but we only have a value of `B`. Again, relying on intuition, we know we should use `b` to generate a value of type `A`. To do so, we can just *materialize* a function `B => A` by asking for it in the signature of `map`:

``` scala
def map[A, B](ca: Codec[A])(f: A => B, g: B => A): Codec[B] = new Codec[B] {
  def encode(b: B): String \/ BitVector =
    ca.encode(g(b))
  ...
}
```

### Detour in to category theory

We now have a working way to convert a `Codec[A]` to a `Codec[B]` but naming the function `map` is awkward. We started with the name map because the problem felt like a functor map. But by the time we finished, we ended up needing two functions, `f: A => B` and `g: B => A`. Let's rename this function to `sortOfMap` for now and consider a typeclass for types that support the general signature:
``` scala
trait NotQuiteFunctor[F[_]] {
  def sortOfMap[A, B](fa: F[A])(f: A => B, g: B => A): F[B]
}
```

[Searching Hoogle](http://www.haskell.org/hoogle/?hoogle=%28a+-%3E+b%29-%3E%28b-%3Ea%29-%3E%28f+a%29-%3E%28f+b%29) for this type signature doesn't find any exact matches. Lars Hupel pointed out to me that this typeclass is actually an *invariant functor* or *exponential functor*. The `sortOfMap` operation is typically called `invmap` or `xmap`. Edward Kmett has a [fascinating blog post exploring exponential functors](http://comonad.com/reader/2008/rotten-bananas/) (note: currently offline, see [cached version](http://webcache.googleusercontent.com/search?q=cache%3Acomonad.com%2Freader%2F2008%2Frotten-bananas%2F&oq=cache%3Acomonad.com%2Freader%2F2008%2Frotten-bananas%2F&aqs=chrome.0.57j58.3499j0&sourceid=chrome&ie=UTF-8)). Additionally, [Tony Morris's index of functor types](http://tmorris.net/posts/functors-and-things-using-scala/index.html) lists the invariant/exponential functor as well.

Now that we know the name for the typeclass we abstracted, let's examine the laws for it. An invariant functor has two laws:

  - identity - `xmap(ma)(identity, identity) == ma`
  - composite - `xmap(xmap(ma)(f1, g1))(f2, g2) == xmap(ma)(f2 compose f1, g1 compose g2)`

The identity law states that xmapping with identity does not modify the input. The composite law states that function composition distributes over the xmap operation.

Note that every *covariant functor* (the kind of functor that us Scala developers are most familiar with) gives rise to an *invariant functor* that ignores the `g` function. Similarly, every *contravariant functor* (which defines `def contramap[A, B](f: B => A): F[B]`) gives rise to an *invariant functor* that ignores the `f` function.

Unfortunately, Scalaz 7 does not provide an `InvariantFunctor` typeclass. It was present in Scalaz 6 though, and as far as I can tell, it wasn't removed deliberately. Hence, we can [integrate it in to the Scalaz 7 typeclass hierarchy](https://github.com/scalaz/scalaz/pull/351).

### Integrating xmap

Now that we have a handle on what this operation is, we can complete the implementation in scodec:
``` scala
object Codec {
  def xmap[A, B](codec: Codec[A])(f: A => B, g: B => A): Codec[B] = new Codec[B] {
    def encode(b: B): String \/ BitVector =
      codec.encode(g(b))
    def decode(buffer: BitVector): String \/ (BitVector, B) =
      codec.decode(buffer).map { case (rest, a) => (rest, f(a)) }
  }
  ...
}
```

We can also add `xmap` directly to the `Codec` trait to allow OO style method usage:

``` scala
trait Codec[A] {
  def encode(a: A): String \/ BitVector
  def decode(b: BitVector): String \/ (BitVector, A)
  final def xmap[B](f: A => B, g: B => A): Codec[B] =
    Codec.xmap(this)(f, g)
}
```

Returning to our original example, we can now easily convert a `Codec[(Int, Int)]` in to a `Codec[Point2D]`:
``` scala
val tuple: Codec[(Int, Int)] = new TupleCodec(int, int)
val point: Codec[Point2D] = tuple.xmap(
  { case (x, y) => Point2D(x, y) },
  { p => (p.y, p.y) }
)
```

### Wrap Up

In this post, we used a type driven approach plus some intuition to implement the xmap operation. By abstracting out a typeclass, we learned about invariant functors.

In the next post, we'll look at using [Shapeless](https://github.com/milessabin/shapeless) to simplify codec definitions and xmapping.

