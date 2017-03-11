+++
title = "scodec - Part 1 - Introduction"
slug = "scodec-intro"
date = "2013-05-27"
categories = [ "articles" ]
tags = [ "scala", "scodec" ]
+++

I recently started the [scodec](http://github.com/mpilquist/scodec) project to simplify handling of binary data in Scala. From the project page:

> This library focuses on contract-first and pure functional encoding and decoding of binary data.
> The following design constraints are considered:
>
>  - Binary structure should mirror protocol definitions and be self-evident under casual reading
>  - Mapping binary structures to types should be statically verified
>  - Encoding and decoding should be purely functional
>  - Failures in encoding and decoding should provide descriptive errors
>  - Compiler plugin should not be used
>
> As a result, the library is implemented as a combinator based DSL.
> Performance is considered but yields to the above design constraints.

This article will introduce some of the main abstractions in scodec and discuss some of the design decisions. Subsequent articles will discuss more interesting parts of scodec, including invariant functors and use of [Shapeless](http://github.com/milessabing/shapeless).

<!--more-->

## Terminology

 - *encoding* - converting a value in to its binary representation
 - *decoding* - converting binary data in to a value
 - *codec* - a value that can encode and decode values of a given Scala type

## Representing Binary

Binary data is passed around a lot in scodec. As such, a representation of binary data is needed. The representation must satisfy a number of constraints:

 - Must be immutable
 - Must support bit operations such as `take(bitCount)`, `drop(bitCount)`, and typical bitwise operators (negation, and, or, xor, ...)
 - Must support fast concatenation in order to handle encoding a number of values in to a single binary structure
 - Must support fast `take(bitCount)`/`drop(bitCount)` operations in order to allow fast decoding

Focusing on individual bits as the fundamental element instead of bytes is the most unique constraint. By focusing on bits instead of bytes or words, the combinator DSL is able to support a more natural description of bit fields.

`Array[Byte]` is an unsuitable representation given that it meets none of the stated constraints. `java.nio.ByteBuffer` does not provide a way to enforce immutable at the type level and fails on the other constraints. `Vector[Byte]` satisfies all constraints except for providing bit operations.

To satisfy these constraints, scodec includes the `BitVector` and `ByteVector` data types and the `BitwiseOperation` supporting trait. This approach allows the API provided to codec authors to be more focused than generic collections. Additionally, the backing implementation of `BitVector` and `ByteVector` can be changed for performance reasons without impacting users.

### BitwiseOperations

`BitwiseOperations` is a supporting trait that's extended by both `BitVector` and `ByteVector`. It provides bitwise shifting operators as well as bitwise negation, bitwise and, bitwise or, and bitwise xor.

``` scala
trait BitwiseOperations[Repr] {

  final def <<(n: Int): Repr = leftShift(n)
  def leftShift(n: Int): Repr

  final def >>(n: Int): Repr = rightShift(n, true)
  final def >>>(n: Int): Repr = rightShift(n, false)
  def rightShift(n: Int, signExtension: Boolean): Repr

  final def unary_~(): Repr = not
  def not: Repr

  final def &(other: Repr): Repr = and(other)
  def and(other: Repr): Repr

  final def |(other: Repr): Repr = or(other)
  def or(other: Repr): Repr

  final def ^(other: Repr): Repr = xor(other)
  def xor(other: Repr): Repr
}
```

### BitVector

`BitVector` represents an immutable sequence of bits. By extending the [`IndexedSeqOptimized[Boolean, BitVector]`](http://www.scala-lang.org/api/current/index.html#scala.collection.IndexedSeqOptimized) trait from the Scala collection library, a bit vector can be used as a collection of booleans. As a bonus, most of the implementation is provided by `IndexedSeqOptimized` - albeit, sometimes inefficiently.

``` scala
trait BitVector extends IndexedSeqOptimized[Boolean, BitVector] with BitwiseOperations[BitVector] {
  ...
  def toByteVector: ByteVector
  def toByteArray: Array[Byte]
  def toByteBuffer: ByteBuffer
}
```

Constructing a bit vector can be done in a number of ways. The `apply` method on the bit vector companion allows direct wrapping of a `ByteVector`, `Array[Byte]`, or `java.nio.ByteBuffer`. To create a bit vector from literal bytes, the literal bytes can be passed directly to apply. For example, `BitVector(0x55, 0x2a)`.

When decoding, many codecs need to take a certain number of bits and convert them to a value. For example, a codec for a signed 32-bit integer might take 32 bits and return those bits converted to an `Int` along with the remaining bits in the input vector (via `input.drop(32)`). `BitVector#take` is insufficient for this task because taking `n` bits from a `m` bit vector where `n > m` results in `m` bits being returned. As a result, every codec would need similar error handling for this case.

To simplify this case, `BitVector` has an `acquire` method, which gets a bit vector of exactly `n` bits or returns an error.
``` scala
def acquire(n: Int): String \/ BitVector = {
  if (size < n) \/ left s"cannot acquire $n bits from a vector that contains $size bits"
  else \/ right take(n)
}
```

Note the use of Scalaz's disjunction (i.e., `\/`) data type, instead of `Option[BitVector]` or throwing an exception. By providing a disjunction that has a descriptive error message, overall error reporting is improved. (In general, I think Scalaz's disjunction is one of the most undervalued parts of Scalaz.)

The `acquire` method only handles getting the necessary bits. Converting the acquired bits to a value may fail. Combining bit acquisition with failure checked type conversion can be refactored in to:

``` scala
def consume[A](n: Int)(decode: BitVector => String \/ A): String \/ (BitVector, A) = for {
  toDecode <- acquire(n)
  decoded <- decode(toDecode)
} yield (drop(n), decoded)
```


### ByteVector

`ByteVector` represents an immutable sequence of bytes.

``` scala
trait ByteVector extends IndexedSeqOptimized[Byte, ByteVector] with BitwiseOperations[ByteVector] {
  ...
  def toArray: Array[Byte]
  def toByteBuffer: ByteBuffer = ByteBuffer.wrap(toArray)
  def toBitVector: BitVector = BitVector(this)
  def toHexadecimal: String
}
```

Unsurprisingly, `ByteVector` is currently implemented by delegating to a `Vector[Byte]`. There are a number of potential improvements, including:

 - for vectors with length <= 8, use a special byte vector implementaiton backed by a single `Long` value
 - delegate to Akka's [`ByteString`](https://github.com/akka/akka/blob/master/akka-actor/src/main/scala/akka/util/ByteString.scala)
 - write a custom vector-like structure that's manually specialized for bytes

Before any of these performance improvements are implemented, benchmarks will be developed in order to measure impacts.

## Codecs

With the basic data types out of the way, we can concretize the definition of a codec.

Encoding is represented by a function `A => String \/ BitVector`. By modeling encoding this way, a failure to encode a value of type `A` can be communicated without throwing an exception. For example, given a `Codec[Int]` that encodes unsigned 24-bit integers, calling encode with a negative integer would return an error message on the left side of the disjunction.

Decoding is represented by a function `BitVector => String \/ (BitVector, A)`. Similar to the rationale for encoding, a failure
to decode a value of type `A` results in an error message on the left side of the disjunction. Successful decoding results in a tuple of the remaining (non-consumed) bits and the decoded value.

``` scala
trait Codec[A] {
  def encode(a: A): String \/ BitVector
  def decode(b: BitVector): String \/ (BitVector, A)
}
```

### Tuple Codec

Consider a codec for a tuple `(A, B)`:

``` scala
class TupleCodec[A, B](codecA: Codec[A], codecB: Codec[B]) extends Codec[(A, B)] {

  override def encode(t: (A, B)) =
    Codec.encodeBoth(codecA, codecB)(t._1, t._2)

  override def decode(buffer: BitVector) =
    Codec.decodeBoth(codecA, codecB)(buffer)
}
```

Let's implement `Codec.encodeBoth` and `Codec.decodeBoth`.

First, encoding:

``` scala
def encodeBoth[A, B](codecA: Codec[A], codecB: Codec[B])(a: A, b: B): String \/ BitVector =
  ???
```

Encoding is fairly straightforward. First, `a` is encoded, then `b` is encoded, and then the resulting vectors are concatenated. Any error results in the overall encoding failing. This is easily accomplished with a for-comprehension:

``` scala
def encodeBoth[A, B](codecA: Codec[A], codecB: Codec[B])(a: A, b: B): String \/ BitVector =
  for {
    encA <- codecA.encode(a)
    encB <- codecB.encode(b)
  } yield encA ++ encB
```

Second, decoding:

``` scala
def decodeBoth[A, B]
  (codecA: Codec[A], codecB: Codec[B])
  (buffer: BitVector): String \/ (BitVector, (A, B)) = ???
```

Decoding is accomplished by first decoding with `codecA`, then, assuming successful decoding, decoding the remaining bits with `codecB`, and finally returning both decoded values in a tuple. Any error results in the overall decoding failing. This could be implemented directly:

``` scala
def decodeBoth[A, B]
  (codecA: Codec[A], codecB: Codec[B])
  (buffer: BitVector): String \/ (BitVector, (A, B)) = {
  val decA = codecA.decode(buffer)
  decA flatMap { case (afterA, a) =>
    val decB = codecB.decode(afterA)
    decB map { case (afterB, b) => (afterB, (a, B)) }
  }
}
```

We can do better than this though. Note the type signature of `Codec#decode`: `BitVector => String \/ (BitVector, A)`. The [`scalaz.StateT`](https://github.com/scalaz/scalaz/blob/scalaz-seven/core/src/main/scala/scalaz/StateT.scala) monad transformer wraps functions of the form `S => F[(S, A)]` for a monad `F`. We can set `F` to `String \/ ?` resulting in `StateT[({type λ[+α] = Error \/ α})#λ, BitVector, A]`. This pattern occurs often enough when working with codecs to warrant special support:

``` scala
type DecodingContext[+A] = StateT[({type λ[+α] = Error \/ α})#λ, BitVector, A]

object DecodingContext {
  def apply[A](f: BitVector => Error \/ (BitVector, A)): DecodingContext[A] =
    StateT[({type λ[+α] = Error \/ α})#λ, BitVector, A](f)
}
```

Returning to `decodeBoth`, we can use `DecodingContext` to implement it simply:

``` scala
def decodeBoth[A, B]
  (codecA: Codec[A], codecB: Codec[B])
  (buffer: BitVector): String \/ (BitVector, (A, B)) = {
  for {
    a <- DecodingContext(codecA.decode)
    b <- DecodingContext(codecB.decode)
  } yield (a, b)
}.run(buffer)
```

## Wrap up

This article presented the basic data types used in scodec. In part 2, we'll look at the `Codec` trait in more detail and investigate how to convert a `Codec[A]` in to a `Codec[B]` given some relations between `A` and `B`.
