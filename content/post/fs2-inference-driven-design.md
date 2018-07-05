---
date: "2018-07-04"
title: "Inference Driven Design"
date: 2018-07-04T15:24:47-04:00
slug: "fs2"
categories: [ "articles" ]
tags: [ "scala", "fp" ]
draft: true
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

## Improving Inference
