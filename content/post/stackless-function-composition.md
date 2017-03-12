+++
date = "2017-03-11"
title = "Stackless Function Composition"
categories = [ "articles" ]
tags = [ "scala", "fp" ]
+++

Last fall I spent a lot of time optimizing the internals of [FS2](https://github.com/functional-streams-for-scala/fs2). The `Free` monad is used extensively in FS2 for modeling various algebras -- most notably, an algebra supporting `Stream` and an algebra supporting `Pull`. Almost any operation a user performs on a `Stream` or `Pull` results in one or more bounces on the trampoline inside `Free`. As such, any optimization in `Free` pays off big time. In this article, we'll look at one of these optimizations -- *map fusion*.

<!--more-->

Operator fusion is the act of condensing successive invocations of an operation in to a single invocation. Hence, map fusion is the act of condensing successive `map` invocations in to a single `map`. In the context of `Free`, map fusion means that for all `fa: Free[F, A]`, `f: A => B`, `g: B => C`, `fa.map(f).map(g)` should result in the *same internal structure* as `fa.map(f andThen g)`. This should result in a significant performance improvement for free programs with lots of successive map operations, especially if `map` is implemented in terms of `flatMap` and `pure`, as we can condense multiple bounces on the trampoline in to a single bounce.

## Coyoneda

There's already a data structure that provides map fusion for an arbitrary type constructor -- `Coyoneda`. Here's a simplified implementation of `Coyoneda` based off the one defined in [Cats](https://github.com/typelevel/cats):

```scala
sealed abstract class Coyoneda[F[_], A] { self =>
  type Pivot
  val pivot: F[Pivot]
  val transform: Pivot => A

  def map[B](f: A => B): Coyoneda[F, B] = new Coyoneda[F, B] {
    type Pivot = self.Pivot
    val pivot = self.pivot
    val transform = f compose self.transform
  }

  def run(implicit F: Functor[F]): F[A] =
    F.map(pivot)(transform)
}

object Coyoneda {
  def apply[F[_], A, B](fa: F[A], f: A => B): Coyoneda[F, B] { type Pivot = A } =
    new Coyoneda[F, B] {
      type Pivot = A
      val pivot: F[Pivot] = fa
      val transform: Pivot => B = f
    }

  def lift[F[_], A](fa: F[A]): Coyondea[F, A] =
    apply[F, A, A](fa, identity)
}
```

`Coyoneda[F, A]` stores a value of `F[X]` for some `X` along with a function from `X => A`. It forms a `Functor[Coyoneda[F, ?]]` where mapping results in extending the transformation function by the function passed to `map`.

In some sense, **`Coyoneda` encodes map fusion directly** -- it hides away an `F[X]` for some `X` and then incrementally builds a transformation function by composing the functions passed to `map`. When composition is complete, the `Coyoneda` is "unwrapped" by mapping the composed transformation over the pivot (`F[X]`) via a single invocation of `map`. We won't go in to detail on the theoretical underpinnings of `Coyoneda`, but if you're interested, a great starting point is [Free Monads and the Yoneda Lemma](http://blog.higher-order.com/blog/2013/11/01/free-and-yoneda/) by Rúnar Bjarnason.

This is pretty powerful -- we can get map fusion for any `Functor` by lifting a value to `Coyoneda`, passing the coyoneda to functions that operate on a `Functor`, and then unwrapping via `run`!

## Embedding Coyoneda in Free

We want to go a bit further with `Free` and embed map fusion inside the `map` method. To do this, we can embed a `Coyoneda` inside `Free`. This is a very mechanical embedding -- the general idea is to add another constructor to `Free` representing `Map` (in addition to the standard constructors like `Pure`, `Suspend`, and `Bind`). The `Map` constructor wraps a `Coyoneda`. We can then implement the `map` operation on `Free` such that it either extends the coyoneda transformation (if `map` was called on a `Map` instance) or constructs a new coyoneda (if `map` was called on any other constructor). So something like this:

```scala
trait Free[F, A] {
  def map[B](f: A => B): Free[F, B] = this match {
    case Map(coyoneda) => Map(coyoneda.map(f))
    case other => Map(Coyondea[Free[F, ?], A, B](other, f))
  }
}
object Free {
  case class Map(coyoneda: Coyoneda[F, A]) extends Free[F, A]
}
```

What do we lose by doing this? With our current definition of `Coyoneda`, we lose stack safety. We normally get stack safety from the monad in which we interpret the `Free`, with `Free` re-associating left-nested `flatMap`s. If `map` is implemented in terms of `flatMap` and `pure`, this works fine. But in the `Coyoneda` backed version, we've lost stack safety due to the dependence on Scala's function composition in `Coyoneda#map`.

Consider:

```scala
val z: Coyoneda[Function0, Int] = Coyoneda(() => 0, identity[Int])

(0 to 10).foldLeft(z)((acc, i) => acc.map(_ + i)).run.apply
// 55

(0 to 10000).foldLeft(z)((acc, i) => acc.map(_ + i)).run.apply
// 50005000

(0 to 100000).foldLeft(z)((acc, i) => acc.map(_ + i)).run.apply
// java.lang.StackOverflowError
//   at scala.Function1.$anonfun$compose$1(Function1.scala:44)
//   at scala.Function1.$anonfun$compose$1(Function1.scala:44)
//   at scala.Function1.$anonfun$compose$1(Function1.scala:44)
```

This is caused by the implementation of `compose` in `scala.Function1`:

```scala
trait Function1[-T1, +R] {
  def apply(v1: T1): R
  def compose[A](g: A => T1): A => R = { x => apply(g(x)) }
  def andThen[A](g: R => A): T1 => A = { x => g(apply(x)) }
}
```

Each call to `compose` (or `andThen`) returns a new function that calls `apply` on the previous function, resulting in a stack frame for each composition.

## Stack Safe Function Composition

Our map fusion optimization hinges on function composition. How can we compose functions in a stack safe way? We can use a *type aligned sequence* -- a sequence of functions such that the output type of each function in the sequence is compatible with the input type of the next function in the sequence. We do not need fancy typelevel functions or dependent types to implement this! Rather, we can use a simple cons list.

```scala
final class ListF1[-A, +B] private (private val fs: List[(Any => Any)]) extends (A => B) {

  override def apply(a: A): B =
    fs.foldLeft(a: Any)((x, f) => f(x)).asInstanceOf[B]

  override def compose[C](g: C => A): ListF1[C, B] =
    new ListF1(g.asInstanceOf[Any => Any] :: fs)
}

object ListF1 {
  def apply[A, B](f: A => B): ListF1[A, B] = f match {
    case f: ListF1[A, B] => f
    case _ => new ListF1(f.asInstanceOf[Any => Any] :: Nil)
  }
}
```

We represent the type level sequence with a `List[(Any => Any)]` and we enforce the "type alignment" property via the type signature of `compose`. The list always has at least one element thanks to `apply`. Each call to `compose` results in cons-ing a function on to the type aligned sequence. Finally, we implement `ListF1#apply` by applying each function in turn, using the output of the previous function as the input to the next.

```scala
// Normal function composition is not stack safe
(0 until 1000000).foldLeft(identity[Int] _)((acc, i) => acc.compose(_ + 1))(0)
// java.lang.StackOverflowError
//   at scala.runtime.java8.JFunction1$mcII$sp.apply(JFunction1$mcII$sp.java:12)
//   at scala.Function1.$anonfun$compose$1(Function1.scala:44)
//   at scala.Function1.$anonfun$compose$1(Function1.scala:44)

// ListF1 composition is stack safe
(0 until 1000000).foldLeft(ListF1(identity[Int]))((acc, i) => acc.compose(_ + 1))(0)
// 1000000
```

This definition is sufficient to restore stack safety to `Coyoneda` and `Free`! We just need to wrap each `Function1` in a `ListF1` inside the definition of `Coyoneda#map`.


## Generalized Stack Safe Function Composition

While `ListF1` provides a stack safe `compose`, it inherits the default `andThen` from `Function1`. This is very dangerous if we hope to provide a general purpose stack safe function wrapper. Extending `ListF1` to support both stack safe `compose` and `andThen` (and arbitrary interleavings) requires us to store the type aligned sequence in a data structure that has constant time cons and snoc operations. The only standard library collection that supports this is `Vector`.

```scala
final class VectorF1[-A, +B] private (private val fs: Vector[(Any => Any)]) extends (A => B) {

  override def apply(a: A): B =
    fs.foldLeft(a: Any)((x, f) => f(x)).asInstanceOf[B]

  override def compose[C](g: C => A): VectorF1[C, B] =
    new VectorF1(g.asInstanceOf[Any => Any] +: fs)

  override def andThen[C](g: B => C): VectorF1[A, C] =
    new VectorF1(fs :+ g.asInstanceOf[Any => Any])
}

object VectorF1 {
  def apply[A, B](f: A => B): VectorF1[A, B] = f match {
    case f: VectorF1[A, B] => f
    case _ => new VectorF1(Vector(f.asInstanceOf[Any => Any]))
  }
}
```

`VectorF1` seems perfect. It has stack safe `compose` and `andThen`. Unfortunately, it is terribly slow. Most composed functions are going to be made up of a small number of compositions -- 2, 3, or 4 functions. It's uncommon to compose 1,000,000 functions. `Vector` has effectively constant time cons and snoc, but there are large constant factors. For example, a single element vector will allocate an 32-element array internally. These constant factors end up dominating the performance results, so this solution won't work.

Fortunately, FS2 has a data structure that performs better -- [`fs2.util.Catenable`](https://oss.sonatype.org/service/local/repositories/releases/archive/co/fs2/fs2-core_2.12/0.9.4/fs2-core_2.12-0.9.4-javadoc.jar/!/fs2/util/Catenable.html). It has O(1) cons, snoc, and concat, amortized O(1) uncons, O(n) traversal, and negligible constant factors for small collections.

```scala
import fs2.util.Catenable

final class CatenableF1[-A, +B] private (private val fs: Catenable[(Any => Any)]) extends (A => B) {

  override def apply(a: A): B =
    fs.foldLeft(a: Any)((x, f) => f(x)).asInstanceOf[B]

  override def compose[C](g: C => A): CatenableF1[C, B] =
    new CatenableF1(g.asInstanceOf[Any => Any] +: fs)

  override def andThen[C](g: B => C): CatenableF1[A, C] =
    new CatenableF1(fs :+ g.asInstanceOf[Any => Any])
}

object CatenableF1 {
  def apply[A, B](f: A => B): CatenableF1[A, B] = f match {
    case f: CatenableF1[A, B] => f
    case _ => new CatenableF1(Catenable.single(f.asInstanceOf[Any => Any]))
  }
}
```

### Stack Safe Function Benchmarks

Let's confirm our performance assumptions with a JMH benchmark:

```scala
@State(Scope.Thread)
class FunctionBenchmark {

  val f = identity[Int] _

  @Benchmark def unitary_Function1 = f(0)
  @Benchmark def unitary_ListF1 = ListF1(f)(0)
  @Benchmark def unitary_VectorF1 = VectorF1(f)(0)
  @Benchmark def unitary_CatenableF1 = CatenableF1(f)(0)

  @Benchmark def compose2_Function1 = (f compose f)(0)
  @Benchmark def compose2_ListF1 = (ListF1(f) compose f)(0)
  @Benchmark def compose2_VectorF1 = (VectorF1(f) compose f)(0)
  @Benchmark def compose2_CatenableF1 = (CatenableF1(f) compose f)(0)

  @Benchmark def andThen2_Function1 = (f andThen f)(0)
  @Benchmark def andThen2_ListF1 = (ListF1(f) andThen f)(0)
  @Benchmark def andThen2_VectorF1 = (VectorF1(f) andThen f)(0)
  @Benchmark def andThen2_CatenableF1 = (CatenableF1(f) andThen f)(0)

  @Benchmark def compose1k_Function1 = (0 until 1000).foldLeft(f)((acc, _) => acc compose f)(0)
  @Benchmark def compose1k_ListF1 = (0 until 1000).foldLeft(ListF1(f))((acc, _) => acc compose f)(0)
  @Benchmark def compose1k_VectorF1 = (0 until 1000).foldLeft(VectorF1(f))((acc, _) => acc compose f)(0)
  @Benchmark def compose1k_CatenableF1 = (0 until 1000).foldLeft(CatenableF1(f))((acc, _) => acc compose f)(0)

  @Benchmark def andThen1k_Function1 = (0 until 1000).foldLeft(f)((acc, _) => acc andThen f)(0)
  @Benchmark def andThen1k_ListF1 = (0 until 1000).foldLeft(ListF1(f): Int => Int)((acc, _) => acc andThen f)(0)
  @Benchmark def andThen1k_VectorF1 = (0 until 1000).foldLeft(VectorF1(f))((acc, _) => acc andThen f)(0)
  @Benchmark def andThen1k_CatenableF1 = (0 until 1000).foldLeft(CatenableF1(f))((acc, _) => acc andThen f)(0)

  @Benchmark def interleaved5k_Function1 = (0 until 5000).foldLeft(f)((acc, i) => if (i % 2 == 0) acc compose f else acc andThen f)(0)
  @Benchmark def interleaved5k_ListF1 = (0 until 5000).foldLeft(ListF1(f): Int => Int)((acc, i) => if (i % 2 == 0) acc compose f else acc andThen f)(0)
  @Benchmark def interleaved5k_VectorF1 = (0 until 5000).foldLeft(VectorF1(f))((acc, i) => if (i % 2 == 0) acc compose f else acc andThen f)(0)
  @Benchmark def interleaved5k_CatenableF1 = (0 until 5000).foldLeft(CatenableF1(f))((acc, i) => if (i % 2 == 0) acc compose f else acc andThen f)(0)
}
```

Running these tests via `jmh:run -i 10 -wi 5 -f1 -t4` results in:

```
[info] Benchmark                                 Mode  Cnt          Score          Error  Units
[info] FunctionBenchmark.andThen1k_CatenableF1      thrpt   10      52645.292 ±     1103.646  ops/s
[info] FunctionBenchmark.andThen1k_Function1        thrpt   10     150172.986 ±    17942.467  ops/s
[info] FunctionBenchmark.andThen1k_ListF1           thrpt   10     146660.259 ±    16294.933  ops/s
[info] FunctionBenchmark.andThen1k_VectorF1         thrpt   10      44720.571 ±     5828.594  ops/s
[info] FunctionBenchmark.andThen2_CatenableF1       thrpt   10   35588572.940 ±  3092474.118  ops/s
[info] FunctionBenchmark.andThen2_Function1         thrpt   10  818744688.563 ± 19275569.749  ops/s
[info] FunctionBenchmark.andThen2_ListF1            thrpt   10  171929071.672 ±  3233542.315  ops/s
[info] FunctionBenchmark.andThen2_VectorF1          thrpt   10   14420360.573 ±   978766.979  ops/s
[info] FunctionBenchmark.compose1k_CatenableF1      thrpt   10      74923.610 ±     2010.544  ops/s
[info] FunctionBenchmark.compose1k_Function1        thrpt   10     212811.294 ±     2892.191  ops/s
[info] FunctionBenchmark.compose1k_ListF1           thrpt   10      77221.242 ±     1649.900  ops/s
[info] FunctionBenchmark.compose1k_VectorF1         thrpt   10      50692.537 ±      901.164  ops/s
[info] FunctionBenchmark.compose2_CatenableF1       thrpt   10   38767734.133 ±   912509.779  ops/s
[info] FunctionBenchmark.compose2_Function1         thrpt   10  814582136.712 ± 12637630.910  ops/s
[info] FunctionBenchmark.compose2_ListF1            thrpt   10   97332398.974 ±  7628692.141  ops/s
[info] FunctionBenchmark.compose2_VectorF1          thrpt   10   11022710.915 ±  2701253.720  ops/s
[info] FunctionBenchmark.interleaved1k_CatenableF1  thrpt   10      48609.388 ±     1223.259  ops/s
[info] FunctionBenchmark.interleaved1k_Function1    thrpt   10     136023.668 ±     9655.060  ops/s
[info] FunctionBenchmark.interleaved1k_ListF1       thrpt   10     163702.682 ±     1764.030  ops/s
[info] FunctionBenchmark.interleaved1k_VectorF1     thrpt   10      25646.788 ±     7765.635  ops/s
[info] FunctionBenchmark.unitary_CatenableF1        thrpt   10   54484868.696 ± 17801651.822  ops/s
[info] FunctionBenchmark.unitary_Function1          thrpt   10  760764365.837 ± 27670122.968  ops/s
[info] FunctionBenchmark.unitary_ListF1             thrpt   10  160625316.286 ± 12419083.383  ops/s
[info] FunctionBenchmark.unitary_VectorF1           thrpt   10   17808062.407 ±  2016163.644  ops/s
```

Based on these tests, `CatenableF1` is about 3x faster than `VectorF1` for small compositions and about 1.5x - 2x `VectorF1` for large collections. `CatenableF1` is about 3x slower than `ListF1` and about 14x slower than `Function1` for small compositions and about equal to `ListF1` and 3x slower than `Function1` for large compositions.

Not bad but stack safety comes at a runtime performance cost. One area for investigation is a dynamic algorithm that switches from `Function1` to `CatenableF1` when the composition stack reaches a certain depth.

## Conclusion

If you check the FS2 source, you might notice that `Free` does not do map fusion. We ended up not merging map fusion in to FS2 because we were able to get comparable performance gains with a simpler optimization that covers more use cases. We now eagerly evaluate `Bind(Pure(a), f)` structures when stepping a `Free`, which gives us the same performance boost as map fusion but covers more use cases and avoids the (minor) constant factor performance hit of `CatenableF1`.
