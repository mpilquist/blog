---
date: "2018-07-04"
title: "Inference Driven Design"
date: 2018-07-04T15:24:47-04:00
slug: "fs2"
categories: [ "articles" ]
tags: [ "scala", "fp" ]
---

Design is an exercise in balancing tradeoffs of various constraints. In this post, we'll look at a library design issue faced in [FS2](https://github.com/functional-streams-for-scala/fs2) and how constraints like Scala's type inference algorithm, minimization of explicit type annotations, and invariance under refactoring can be balanced.

<!--more-->

A `Stream[F, A]` is a pure data type that describes a program which can evaluate effects in the type constructor `F` and emit zero or more values of type `A`. We can start a sketch of this in Scala like so:

```scala
import scala.language.higherKinds

trait Stream[F[_], A]

object Stream {
  def emits[F[_], A](as: List[A]): Stream[F, A] = new Stream[F, A] {}
  def eval[F[_], A](fa: F[A]): Stream[F, A] = new Stream[F, A] {}
  def append[F[_], A](x: Stream[F, A], y: Stream[F, A]): Stream[F, A] = new Stream[F, A] {}
}
```

The `emits` constructor takes a pure list of type `A` and returns a `Stream[F, A]` which emits each value in the list in order. The `eval` constructor takes a value of `F[A]` and returns a `Stream[F, A]` that, when interpreted, evaluates the `F[A]` and emits the single result value. The `append` constructor creates a stream from two other streams by first evaluating and emitting the first stream and upon reaching its end, evaluating and emitting the second stream. We're eliding the implementation of these constructors in this sketch as we won't need them to explore the design space.

Note that the `F` type parameter of `emits` is unconstrained -- the compiler is free to infer any type constructor for `F` because it isn't used in the parameter list and the `Stream` trait places no constraints on it.

Let's try to use our new `Stream` type:

```scala
Stream.append(Stream.emits(List(1)), Stream.eval(IO(1)))
```

Compiling this with Scala 2.12.6 results in the following error:

```scala
01.scala:17: error: no type parameters for method append: (x: Stream[F,A], y: Stream[F,A])Stream[F,A] exist so that it can be applied to arguments (Stream[Nothing,Int], Stream[IO,Int])
 --- because ---
argument expression's type is not compatible with formal parameter type;
 found   : Stream[Nothing,Int]
 required: Stream[?F,?A]

  Stream.append(Stream.emits(List(1)), Stream.eval(IO(1)))
         ^
01.scala:17: error: type mismatch;
 found   : Stream[Nothing,Int]
 required: Stream[F,A]
  Stream.append(Stream.emits(List(1)), Stream.eval(IO(1)))
                            ^
01.scala:17: error: type mismatch;
 found   : Stream[IO,Int]
 required: Stream[F,A]
  Stream.append(Stream.emits(List(1)), Stream.eval(IO(1)))
                                                  ^
three errors found
```

Eek! What's going on here? It seems like the compiler chose `F = Nothing` when inferring the type parameters of `Stream.emits`. It then complains that `append` wants two streams that have the same effect type but it was called with `F = Nothing` for the first param and `F = IO` for the second param.

I suspect the truth is a little bit more complicated. Internally, scalac uses `Nothing` to represent a type parameter that has not yet been determined -- a placeholder for a type that needs to be resolved. During type checking, these nothings are replaced by the computed types. So what happened in this case? Roughly, we have a tree like this:

```scala
Stream.append[T₀, T₁](Stream.emits[T₂, T₃](List[T₄](1)), Stream.eval[T₅, T₆](IO[T₇](1)))
```

Each type param here is labeled with a unique name. When typing this expression, the compiler will attempt to find a type for each `T` that makes the overall expression well typed. Rather than simulate the mechanism by which scalac types this, we can type it informally. First, let's look at `List[T₄](1)`. The value `1` has type `Int` so based on the definition of `List.apply`, `T₄ = Int`. Similarly, we can determine `T₇ = Int` by looking at the expression `IO[T₇](1)`. We can then look at `Stream.eval[T₅, T₆](IO[Int](1))`, coupled with the definition of `Stream.eval`, to determine that `T₅ = IO` and `T₆ = Int`.

Moving on, we can look at `Stream.emits[T₂, T₃](List[Int](1))`. `T₃` must be `Int` based on the definition of `emits` but there's no information to help us determine T₂, so we'll leave that unsolved. Putting together what we've determined so far gives us this:

```scala
Stream.append[T₀, T₁](Stream.emits[T₂, Int](List[Int](1)), Stream.eval[IO, Int](IO[Int](1)))
```

Using the definition of `append`, we can say that `T₁ = Int` because both parameters to `append` have type `Int` in the `A` position of `Stream`. Using a similar procedure for `T₀` results in the equalities `T₀ = T₂ = IO`. There are no further type parameters so we have successfully inferred and type checked this expression. Note how the type of the second parameter to `append` helped compute the unconstrained type parameter used in the first parameter to `append`.

So where does scalac fail? I think it gets confused in its tracking of unresolved type parameters due to its dual use of `Nothing` -- the notion that a type parameter is known to be `Nothing` and the notion that a type parameter has not yet been determined. The confusion only occurs when the undetermined type parameter is a higher kinded type.

Dotty / Scala 3 does not use `Nothing` to represent an undetermined type parameter. Not conincidentally, this expression type checks fine in Dotty.

So what can we do? One option is to help the compiler by explicitly supplying `T₂ = IO`. We have to specify `T₃ = Int` as well because Scala doesn't provide syntax for supplying just some of the type paramters of a polymorphic method. This ends up looking like:


```scala
Stream.append(Stream.emits[IO, Int](List(1)), Stream.eval(IO(1)))
```

This type checks under both Scala 2.12.6 and Dotty. Unfortunately, explicitly providing such types does not scale. FS2 makes heavy use of composition, so it's common to have expressions consisting of many individual calls. Explicitly providing types in large expressions ends up being extremely verbose.

Furthermore, even if this bug was fixed (like in Dotty), relying on unconstrained type parameters getting inferred from the surrounding context can be confusing due to Scala's local type inference. We can use Dotty to show this by extracting the `Stream.emits` call to a local val:

```scala
val x = Stream.emits(List(1))
Stream.append(x, Stream.eval(IO(1)))
```

Dotty complains:

```
-- [E007] Type Mismatch Error: 01.scala:27:31 ----------------------------------
27 |Stream.append(x, Stream.eval(IO(1)))
   |                             ^^^^^
   |                             found:    IO[Int]
   |                             required: Nothing
   |
one error found
```

Type inference happens on each statement independently, so `val x = Stream.emits(List(1))` is typed first, resulting in `Stream[Nothing, Int]`. Dotty then types the `Stream.append` expression. In doing so, the `Stream.eval(IO(1))` subexpresion is expanded to `Stream.eval[T₅, T₆](IO[Int](1))` and `T₅` is assigned to `Nothing` due to `x: Stream[Nothing, Int]`. Dotty reports a type mismatch because the parameter to `eval` has type `IO[Int]` but is expected to have type `Nothing` due to `T₅ = Nothing`.

If Scala supported the notion of polymorphic values, we could avoid this problem while keeping local type inference. Specifically, `x` could be typed as `[F[_]] Stream[F, Int]` instead of `Stream[Nothing, Int]`. Excuse the pseudo-syntax here -- what we're trying to express is that `x` is a `Stream[F, Int]` for all type constructors `F`. Scala 2 doesn't support polymorphic values though and it's [unlikely that Dotty will](https://github.com/lampepfl/dotty/pull/4672#issuecomment-398950818) (thanks to Guillaume Martres for the link).

This issue is a known consequence of Scala's local type inference and lack of polymorphic values - it's even mentioned in the [Tour of Scala](https://docs.scala-lang.org/tour/local-type-inference.html). However, we'd really like our programs to be invariant under certain syntatic refactorings like extracting a subexpression to a val. Hence, we need to consider the implications of type inference on library design.

## Parametricity of Polymorphic Streams (or Intentional Nothings)

Before we look at a solution to our inference problems, let's consider a few interesting stream types. Let's first consider what happens when we pass `Nil` to `emits`. We get back a `[F[_], A] Stream[F, A]` (in a fantasy Scala that has polymorphic values). By parametricity, we know this stream cannot evaluate any effects (because it's polymorphic in `F`) and it cannot emit any values (because it's polymorphic in `A`). In order for the stream to output a value, it needs to know how to instantiate an `A`, but the stream is defined for all `A` and we don't provide a way to generate an `A`. Hence it's impossible for the stream to output a value. A similar argument holds for effect evaluation.

If we pass a non-empty list of `Int` to `emits`, we get back a `[F[_]] Stream[F, Int]`. By parametricity, we know that this stream cannot evaluate any effects. Pure streams are interesting -- since they don't evaluate effects, we can fold them directly to a pure value instead of first interpreting them to an effect type of `F`. We can also combine them with any other stream of a known effect type.

Let's imagine a new function called `drain` which ignores all the emitted values of a stream, again using fantasy polymorphic value syntax:
```scala
def drain[F[_], A](s: Stream[F, A]): [B] Stream[F, B] = ???
```
The `drain` function returns a new stream that when interpreted, still evaluates effects in order but drops/ignores the emitted values. Because it doesn't output any values, we model the result as a stream that's polymorphic in its output type.

Given that we don't have polymorphic values, such expressions are going to result in `Nothing` getting inferred for the unconstrained type parameters. Hence, we can analyze these three polymorphic stream types under such a substitution. Doing so gives us an empty stream of type `Stream[Nothing, Nothing]`, a pure stream that outputs values of `A` type `Stream[Nothing, A]`, and a stream that evaluates effects in `F` and outputs nothing (sic) type `Stream[F, Nothing]`.

## Improving Inference

Can we reify these `Nothing` streams with normal streams? For example, can we get `append` to support appending a pure stream with a normal stream? A normal stream with a pure stream? A pure stream with an empty stream? Etc.

One way to do this is to use subtyping and variance. First, let's consider the output type. The general idea is that a stream is very much like a list - it emits 0 or more values similar to the way a list emits 0 or more values. A list is covariant in its type parameter and similarly, a stream is covariant in its output type parameter. We can model this as `trait Stream[F[_], +A]`. `Nothing` is the bottom type in Scala -- it is a subtype of every other type -- which means a `Stream[F, Nothing]` is a subtype of `[A] Stream[F, A]`. Similarly, we can define stream to be covariant with respect to the effect type as well: `trait Stream[+F[_], +A]`, which means `Stream[Nothing, Nothing]` is a subtype of `[F[_], A] Stream[F, A]`.

By defining stream to be covariant with respect to both type parameters, both Scala 2.12.6 and Dotty can compile all of the following examples:

```scala
val r = Stream.append(Stream.emits(List(1)), Stream.eval(IO(1)))

val s = Stream.emits(List(1))
val t = Stream.eval(IO(1))
val u = Stream.append(s, t)

val x = Stream.emits(List(1))
val y = Stream.append(x, Stream.eval(IO(1)))
```

In some sense, variance is letting us paper over the inference bug we saw in 2.12.6!

### Syntax

Let's define an infix alias for `Stream.append`:

```scala
trait Stream[+F[_], +A] {
  def ++(that: Stream[F, A]): Stream[F, A] = Stream.append(this, that)
}
```

This fails to compile due to our variance annotations:

```scala
-- Error: 02.scala:4:13 --------------------------------------------------------
4 |  def append(that: Stream[F, A]): Stream[F, A] = Stream.append(this, that)
  |             ^^^^^^^^^^^^^^^^^^
  |covariant type F occurs in contravariant position in type Stream[F, A] of value that
one error found
```

There are two standard ways to fix this. One way is to introduce type parameters for each covariant type:

```scalac
trait Stream[+F[_], +A] {
  def ++[F2[x] >: F[x], A2 >: A](that: Stream[F2, A2]): Stream[F2, A2] = Stream.append(this, that)
}
```

Another approach is to define the infix method as an extension method, forgetting about variance in the process:

```scalac
trait Stream[+F[_], +A]
object Stream {
  implicit class InvariantOps[F[_], A](private val self: Stream[F, A]) extends AnyVal {
    def ++(that: Stream[F, A]): Stream[F, A] = Stream.append(self, that)
  }
}
```

Both of these options have pros and cons. The method signature of the first solution is complex and hard to read whereas the extension method is sort of hidden from the reader of the code and sometimes doesn't play well with ScalaDoc (even with `-implicits` flag). The former introduces more type parameters, which means we lean on the compiler more to infer types (and perhaps we run in to more inference bugs in Scala 2.12). The latter relies on an implicit conversion to add the extension method, which means we have a [potential for conflicts with other implicit conversions](https://github.com/functional-streams-for-scala/fs2/issues/1169).

So far, it seems like either approach is fine.

### Through Combinator

Let's add a new combinator to `Stream` called `through`, which applies a function to the stream. We'll define it as both a direct method (`dthrough`) and an extension method (`ethrough`).

```scala
trait Stream[+F[_], +A] {
  def dthrough[F2[x] >: F[x], A2 >: A, B](p: Stream[F2, A2] => Stream[F2, B]): Stream[F2, B] = p(this)
}
object Stream {
  implicit class InvariantOps[F[_], A](private val self: Stream[F, A]) extends AnyVal {
    def ethrough[B](p: Stream[F, A] => Stream[F, B]): Stream[F, B] = p(self)
  }
}
```

Now let's try some usages. First, let's try calling `dthrough(identity)` on a `Stream[IO, Int]`:

```scala
val a: Stream[IO, Int] = Stream.eval(IO(1))
a.dthrough(identity)
```

Scala 2.12 fails to compile (Dotty passes):

```
03.scala:42: error: polymorphic expression cannot be instantiated to expected type;
 found   : [A]A => A
 required: Stream[?,?] => Stream[?,?]
a.dthrough(identity)
           ^
one error found
```

Let's try the same example with `ethrough`:

```scala
val a: Stream[IO, Int] = Stream.eval(IO(1))
a.ethrough(identity)
```

This one works fine under both Scala 2.12 and Dotty.

Now let's try both `dthrough` and `ethrough` with an unconstrained type parameter:

```scala
Stream.emits(List(1)).dthrough(identity)
Stream.emits(List(1)).ethrough(identity)
```

Both of these expressions fail to compile under Scala 2.12 and both work fine under Dotty.

```scala
03.scala:45: error: polymorphic expression cannot be instantiated to expected type;
 found   : [A]A => A
 required: Stream[?,?] => Stream[?,?]
Stream.emits(List(1)).dthrough(identity)
                               ^
03.scala:46: error: polymorphic expression cannot be instantiated to expected type;
 found   : [A]A => A
 required: Stream[?,Int] => Stream[?,?]
Stream.emits(List(1)).ethrough(identity)
                               ^
two errors found
```

Hmm, seems like the extension method approach works better in this case but both encodings fail in presence of unconstrained type parameters.

## A Pure Encoding

In FS2, we addressed this problem by avoiding unconstrained type parameters and using extension methods for invariant operations. Let's see how this works in two steps. First, we'll change all the constructors to avoid unconstrained type parameters & use of `Nothing`. In this case, that means changing the definition of `emits` but in FS2, there are a few dozen derived constructors that need this treatment. We'll keep the `Stream` type covariant in both type parameters so the compiler can use the approrpiate subtype relationships when it is able, but we'll also introduce a special effect type that serves the same purpse as `Nothing` in this use case, called `Pure`.

We'll need the ability to widen `[F[_], A] Stream[Pure, A] => Stream[F, A]` -- we call this operation `covary`. We can implement `covary` simply by casting, because we define `Pure` to be an uninhabited type -- meaning we have a guarantee that there are no "eval" nodes in a `Stream[Pure, A]`. We'll also need an implicit covary so that the compiler is free to convert a pure stream to an effectful stream at any point.

By encoding invariant operations as extension methods, we're able to provide "overloads" or alternative extension methods specialized for `Stream[Pure, ?]`.

```scala
import scala.language.higherKinds
import scala.language.implicitConversions

sealed trait Pure[A]

trait Stream[+F[_], +A]

object Stream {
  def emits[A](as: List[A]): Stream[Pure, A] = new Stream[Pure, A] {}
  def eval[F[_], A](fa: F[A]): Stream[F, A] = new Stream[F, A] {}
  def append[F[_], A](x: Stream[F, A], y: Stream[F, A]): Stream[F, A] = new Stream[F, A] {}

  implicit def covaryPure[F[_], A](s: Stream[Pure, A]): Stream[F, A] = s.covary[F]

  implicit class InvariantOps[F[_], A](private val self: Stream[F, A]) extends AnyVal {
    def ++(that: Stream[F, A]): Stream[F, A] = Stream.append(self, that)
    def through[B](p: Stream[F, A] => Stream[F, B]): Stream[F, B] = p(self)
    def covary[F2[x] >: F[x]]: Stream[F2, A] = self.asInstanceOf[Stream[F2, A]]
  }

  implicit class PureOps[A](private val self: Stream[Pure, A]) extends AnyVal {
    def ++[F[_]](that: Stream[F, A]): Stream[F, A] = Stream.append(self, that)
    def covary[F[_]]: Stream[F, A] = self.asInstanceOf[Stream[F, A]]
  }
}
```

With this encoding, all of the following expressions work under 2.12.6 (and 2.11.12):

```scala
Stream.emits(List(1)) ++ Stream.eval(IO(1))
Stream.eval(IO(1)) ++ Stream.emits(List(1))
Stream.emits(List(1)).through(identity)
```

This encoding is pretty heavyweight. For every invariant operation, we have to define 2 overloads - one for an arbitrary `F` and one for `Pure`. Oddly, it's the only example so far for which Dotty fails -- though dotc crashes when compiling this example so that's likely just a bug. This encoding only exists to work around inference bugs in Scala 2.12 (and prior). While the encoding is heavyweight, it's boilerplate that FS2 authors have to deal with, resulting in much better type inference in user code.

There are likely better solutions to this problem. If you have an idea, please discuss with us on the [FS2 issue tracker](https://github.com/functional-streams-for-scala/fs2/issues).
