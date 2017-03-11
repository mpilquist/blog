+++
date = "2013-06-09"
title = "scodec - Part 3 - Shapeless"
slug = "scodec-part-3"
categories = [ "articles" ]
tags = [ "scala", "scodec" ]
+++

This is part 3 of a series of posts about [scodec](http://github.com/mpilquist/scodec/). In [Part 2](/blog/2013/06/01/scodec-part-2/), we saw how to create a `Codec[A]` and xmap it in to a `Codec[B]`. In this post, we'll show how to use [Shapeless](http://github.com/milessabin/shapeless/) to perform automatic xmapping to case classes.

<!--more-->

# Shapeless HLists

Shapeless is a generic programming library by Miles Sabin. One of the primary abstractions in Shapeless is the HList, or heterogeneous list. An HList represents a list of values of arbitrary types where the type of each element is preserved during compilation. For example:
``` scala
val foo = 1 :: true :: "yes" :: Nil
val bar = 1 :: true :: "yes" :: HNil
```
The type of `foo` is `List[Any]` because `Any` is the least upper bound of `Int`, `Boolean`, and `String`. The type of `bar` is `Int :: Boolean :: String :: HNil`. The number of list elements and the type of each element is preserved.

For a fantastic introduction to HLists, see Miles's [recent talk at flatMap(Oslo)](http://2013.flatmap.no/sabin.html).

Shapeless supports converting an HList of the proper type to a case class instance and vice-versa. For example:
``` scala
case class Point3D(x: Int, y: Int, z: Int)

val pointIso: Iso[Point3D, Int :: Int :: Int :: HNil] =
  Iso.hlist(Point3D.apply _, Point3D.unapply _)

val components = 1 :: 2 :: 3 :: HNil
val point = pointIso.from(components) // Point(1, 2, 3)
val backToComponents = pointIso.to(point) // 1 :: 2 :: 3 :: HNil
```

The key abstraction here is `Iso`, which represents an isomorphism between two types. `Iso` is defined in Shapeless source like this:
``` scala
trait Iso[T, U] { self =>
  def to(t : T) : U
  def from(u : U) : T
  ...
}
```

The `Iso` companion object defines the `hlist` method, which returns an isomorphism between a case class and an HList of the appropriate shape given the case class's `apply` and `unapply` methods. Typically, the iso is defined as an implicit val in the companion object of the case class to allow other modules implicit access to it. Additionally, it's type is typically inferred. So we'd actually write this:
``` scala
case class Point3D(x: Int, y: Int, z: Int)

object Point3D {
  implicit val hlistIso = Iso.hlist(Point3D.apply _, Point3D.unapply _)
}
```

(Note: the [macro-paradise](https://github.com/milessabin/shapeless/tree/topic/macro-paradise) branch of Shapeless removes the need for this boilerplate. Instead, an equivalent isomorphism can be summoned when needed via `Iso[Point3D, Int :: Int :: Int :: HNil]`. The macro-paradise branch uses new macro features from [Eugene Burmako's branch of Scala](http://docs.scala-lang.org/overviews/macros/paradise.html). For more information, see [Miles's talk at NEScala 2013](http://marakana.com/s/post/1421/shapeless_meets_implicit_macros). For a discussion on new macro features, see [scala-internals](https://groups.google.com/d/msg/scala-internals/91W0-PxMQ9Q/bTounkiouB8J).)

# HList Codecs

Given a codec for an HList and an iso between the HList type and a case class, we can use `xmap` to convert the codec:

``` scala
val comps: Codec[Int :: Int :: Int :: HNil] = ???
val points: Codec[Points3D] = comps.xmap(Point3D.hlistIso.from, Point3D.hlistIso.to)
```

This is an extremely common operation, so we let's add direct support for it. We can add a method to `Codec` that takes an `Iso` and delegates to `xmap`. Further, we can make the iso parameter implicit, allowing the compiler to find the right iso automatically:

``` scala
trait Codec[A] {
  def encode(a: A): Error \/ BitVector
  def decode(bits: BitVector): Error \/ (BitVector, A)
  ...
  final def as[B](implicit iso: Iso[B, A]): Codec[B] = Codec.xmap(this)(iso.from, iso.to)
}
```

This allows our point example to be rewritten as:
``` scala
val comps: Codec[Int :: Int :: Int :: HNil] = ???
val points: Codec[Points3D] = comps.as[Points3D]
```

# Constructing HList Codecs

So once we have a codec for an HList, we can easily convert it to a codec for a case class, but how can we create an HList codec? There's always the option to extend `Codec` directly but we'd prefer something more compositional in nature -- some way to create the codec from smaller codecs.

## HList of Codecs

One way of accomplishing this is to create an HList of codecs:

``` scala
val int: Codec[Int] = ...
val threeInts = int :: int :: int :: HNil
```

The type of `threeInts` is `Codec[Int] :: Codec[Int] :: Codec[Int] :: HNil` but we want a `Codec[Int :: Int :: Int :: HNil]`. It looks like an [applicative sequence operation on the HList](http://stackoverflow.com/questions/16127360/sequencing-an-hlist) would do the trick but there's no `Applicative` instance for `Codec` (remember in part 2, we showed that `Codec` does not have a covariant functor instance, therefore, there cannot be an `Applicative` instance). Instead, we can implement this conversion directly by folding right over the elements of the HList. Our starting value is simply a `Codec[HNil]` and the fold operation prepends a specific codec on to the accumulated HList codec.

Let's start by implementing the empty case and the fold operation:

``` scala
object HListCodec {

  val hnilCodec: Codec[HNil] = new Codec[HNil] {
    def encode(hn: HNil) = \/-(BitVector.empty)
    def decode(buffer: BitVector) = \/-((buffer, HNil))
  }

  def prepend[A, L <: HList](a: Codec[A], l: Codec[L]): Codec[A :: L] =
    new Codec[A :: L] {
      override def encode(xs: A :: L) = Codec.encodeBoth(a, l)(xs.head, xs.tail)
      override def decode(buffer: BitVector) = (for {
        decA <- Codec.DecodingContext(a.decode)
        decL <- Codec.DecodingContext(l.decode)
      } yield decA :: decL).run(buffer)
    }
}
```

The `prepend` method's type signature is interesting. Every specific HList (e.g., `Int :: Int :: Int :: HNil`) is a subtype of the HList type. The signature of `prepend` takes advantage of that in order to take a `Codec[L]` where `L` is any HList type. Further, it uses `L` in its return type, along with the cons (`::`) type operator, to declare that a `Codec[A :: L]` is returned. As a result, `encode` can split its argument in to an `A` and an `L` via `head` and `tail` respectively, and use the `encodeBoth` function we implemented in part 1. Similarly, `decode` can decode an `A` and an `L` and then put them together with the cons value operator.

Now let's use these with `foldRight`:

``` scala
object HListCodec {
  ...
  def apply[L <: HList](l: L) =
    l.foldRight(hnilCodec)(prepend)
}
```

There are two issues with this implementation though. The first is that `foldRight` on HList takes a `Poly2` as its second argument, not a regular monomorhpic function. Second, our type signature claims to work with all HLists, regardless of their contents, but we only want to accept `HLists` where each element is a `Codec` of an arbitrary type.

Let's lift `prepend` in to a `Poly2` instance:

``` scala
object HListCodec {
  ...
  object Prepend extends Poly2 {
    implicit def caseCodecAndCodecHList[A, L <: HList] =
      at[Codec[A], Codec[L]](prepend)
  }
}
```

Now we need to limit the type of `HLists` that can be passed to apply. We can do this by asking (the compiler) for evidence that each member of `L` is a `Codec` of an arbitrary type. Shapeless supports this via `UnaryTCConstraint`:
``` scala
object HListCodec {
  ...
  import UnaryTCConstraint._
  def apply[L <: HList : *->*[Codec]#λ](l: L) =
    l.foldRight(hnilCodec)(Prepend)
}
```

Compiling this shows that we aren't quite done:
`could not find implicit value for parameter folder: shapeless.RightFolder[L,scodec.Codec[shapeless.HNil],scodec.HListCodec.Prepend.type]`

This is due to the `foldRight` operation requiring an implicit `RightFolder` parameter. In general, when can solve these types of errors by adding implicit parameters to our function signature. In this case, by requiring an implicit `RightFolder` of the specified type:

``` scala
object HListCodec {
  ...
  import UnaryTCConstraint._
  def apply[L <: HList : *->*[Codec]#λ](l: L)(
    implicit folder: RightFolder[L, Codec[HNil], Prepend.type]
  ) = l.foldRight(hnilCodec)(Prepend)
}
```

We can stop at this point if we want - the implementation is now fully functional. If we want to be explicit about the return type of `apply`, we have one more step. Namely, we need to indicate that `apply` returns a `Codec[M]` where `M` is an HList. Further, we need to describe the relationship between `L` and `M`, which is provided by `RightFolderAux` - an alternative to `RightFolder` that lets us use the result type:

``` scala
object HListCodec {
  ...
  import UnaryTCConstraint._
  def apply[L <: HList : *->*[Codec]#λ, M <: HList](l: L)(
    implicit folder: RightFolderAux[L, Codec[HNil], Prepend.type, Codec[M]]
  ): Codec[M] = l.foldRight(hnilCodec)(Prepend)
}
```

## Prepending a codec on to an HList codec

Now that we have a way to construct HList codecs, let's consider other operations on them. For starters, consider combining a `Codec[A]` with a `Codec[L]` where `L` is an HList. We built this combinator earlier -- the `prepend` method used in the call to `foldRight` when constructing an HList codec. We can add an operator to HList codecs that aliases `prepend`:

``` scala
implicit class EnrichedHListCodec[L <: HList](l: Codec[L]) {
  def ::[A](a: Codec[A]): Codec[A :: L] = HListCodec.prepend(a, l)
}
```

Now we can cons codecs on the the front of an HList codec with `::`. For example:

``` scala
val comps = int :: int :: int :: HListCodec.hnilCodec
```

We can further simplify this by defining the `::` operator on a non-HList codec as well. Calling `::` on a non-HList codec should prepend the left codec on to the result of the right codec prepended to `Codec[HNil]`:

``` scala
implicit class EnrichedCodec[A](codecA: Codec[A]) {
  def ::[B](codecB: Codec[B]): Codec[B :: A :: HNil] =
    HListCodec.prepend(codecB, HListCodec.prepend(codecA, HListCodec.hnilCodec))
}
```

This lets us write the `Points3D` codec as:
``` scala
val points = (int :: int :: int).as[Points3D]
```

## Appending a codec on to an HList codec

Similarly, we can imagine the need to append a `Codec[A]` on to the end of a `Codec[L]`. We don't have a convenient type operator to represent the result of appending `A` to `L` so we'll just define a new type parameter `LA` and require implicit evidence that ensures `LA` represents the list with `A` appended to the end of `L`.

``` scala
object HListCodec {
  def append[L <: HList, A, LA <: HList](
    l: Codec[L], a: Codec[A]
  )(implicit ???
  ): Codec[LA] = new Codec[LA] {
    override def encode(xs: LA) = ???
    override def decode(buffer: BitVector) = ???
  }
}
```

Let's start with the implementation of decode. We can decode with `l` and then with `a` and then append the result from `a` on to the end of the result from `l`. For the implementation of encode, we need to take everything but the last element out of the provided value of type `LA` and encode it with `l`. Then we need to take the last element from the value of type `LA` and encode it with `a`:

``` scala
object HListCodec {
  def append[L <: HList, A, LA <: HList](
    l: Codec[L], a: Codec[A]
  )(implicit ???
  ): Codec[LA] = new Codec[LA] {
    override def encode(xs: LA) = Codec.encodeBoth(l, a)(xs.init, xs.last)
    override def decode(buffer: BitVector) = (for {
      decL <- Codec.DecodingContext(l.decode)
      decA <- Codec.DecodingContext(a.decode)
    } yield decL :+ decA).run(buffer)
  }
}
```

Compiling this and inspecting the implicits that Shapeless needs causes us to add an implicit for the call to `init`, and implicit for the call to `last`, and an implicit for the call to `:+`. For the implicits supporting `init` and `last`, we need to further refine the return type of each by specifying the output type:

``` scala
object HListCodec {
  def append[L <: HList, A, LA <: HList](l: Codec[L], a: Codec[A])(implicit
    prepend: PrependAux[L, A :: HNil, LA],
    init: Init[LA] { type Out = L },
    last: Last[LA] { type Out = A }
  ): Codec[LA] = new Codec[LA] {
    override def encode(xs: LA) = Codec.encodeBoth(l, a)(xs.init, xs.last)
    override def decode(buffer: BitVector) = (for {
      decL <- Codec.DecodingContext(l.decode)
      decA <- Codec.DecodingContext(a.decode)
    } yield decL :+ decA).run(buffer)
  }
}
```

And we can provide syntax support for this as well:

``` scala
implicit class EnrichedHListCodec[L <: HList](l: Codec[L]) {
  ...
  def :+[A, LA <: HList](a: Codec[A])(implicit
    prepend: PrependAux[L, A :: HNil, LA],
    init: Init[LA] { type Out = L },
    last: Last[LA] { type Out = A }
  ): Codec[LA] = append(l, a)
}
```

## Concatenating HList codecs

Finally, we can concatenate two HList codecs. We can concatenate a `Codec[K]` with a `Codec[L]` in to a `Codec[KL]`. In the same way we did with the `append` method, we'll just invent the type parameter `KL` and then require evidence that it has the structure of `K` concatenated with `L`:

``` scala
def concat[K <: HList, L <: HList, KL <: HList](ck: Codec[K], cl: Codec[L])(
  implicit ???
): Codec[KL] = new Codec[KL] {
  override def encode(xs: KL) = ???
  override def decode(buffer: BitVector) = ???
}
```

As usual, we'll start with the implementation of decode. We can use the same basic template as we used in `append` but instead of appending a single element, we'll just concatenate the result of each decode operation. The compiler informs us that we need a `Prepend` implicit in scope. This time, we want to bind the result of concatenation to the `KL` type parameter so we'll use `PrependAux` instead:

``` scala
def concat[K <: HList, L <: HList, KL <: HList, KLen <: Nat](ck: Codec[K], cl: Codec[L])(implicit
  prepend: PrependAux[K, L, KL]
): Codec[KL] = new Codec[KL] {
  override def encode(xs: KL) = ???
  override def decode(buffer: BitVector) = (for {
    decK <- Codec.DecodingContext(ck.decode)
    decL <- Codec.DecodingContext(cl.decode)
  } yield decK ::: decL).run(buffer)
}
```

To encode, we need to split the provided HList of type `KL` in to two HLists -- one of `K` and one of `L`. A feature of HLists is that they encode their length in to their static type. Hence, we can get the length of `K` and split the provided `KL` at that position, yielding two HLists -- one of type `K` and another of type `L`. Because we are splitting `KL` with a statically known length, the `split` method needs a type encoding of the length. Hence, we need to add another type parameter to the method that represents the length of list `K` - called 'KLen'. We then need to bind the length of `K` to that parameter. Finally, we need an implicit `Split` parameter for splitting `KL` at position `KLen`:

``` scala
def concat[K <: HList, L <: HList, KL <: HList, KLen <: Nat](
  ck: Codec[K], cl: Codec[L]
)(implicit
  prepend: PrependAux[K, L, KL],
  lengthK: Length[K] { type Out = KLen },
  split: Split[KL, KLen] { type P = K; type S = L }
): Codec[KL] = new Codec[KL] {
  override def encode(xs: KL) = {
    val (k, l) = xs.split[KLen]
    Codec.encodeBoth(ck, cl)(k, l)
  }
  override def decode(buffer: BitVector) = (for {
    decK <- Codec.DecodingContext(ck.decode)
    decL <- Codec.DecodingContext(cl.decode)
  } yield decK ::: decL).run(buffer)
}
```

Syntax support follow the same pattern as before:

``` scala
implicit class EnrichedHListCodec[L <: HList](l: Codec[L]) {
  def :::[K <: HList, KL <: HList, KLen <: Nat](k: Codec[K])(implicit
    prepend: PrependAux[K, L, KL],
    lengthK: Length[K] { type Out = KLen },
    split: Split[KL, KLen] { type P = K; type S = L }
  ): Codec[KL] = concat(k, l)
}
```

# Wrap Up

In this post we looked at how scodec uses Shapeless to provide type-safe binding of binary structures to case classes. In the process, we've explored some type-level generic programming.
