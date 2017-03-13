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

While `ListF1` provides a stack safe `compose`, it inherits the default `andThen` from `Function1`. This is very dangerous if we hope to provide a general purpose stack safe function wrapper. Extending `ListF1` to support both stack safe `compose` and `andThen` (and arbitrary interleavings) requires us to store the type aligned sequence in a data structure that has constant time cons and snoc operations. The only standard library collection that supports this is `Vector` (correction: [@nickstanch pointed out that `scala.collection.immutable.Queue` has O(1) cons and snoc and O(n) traversal](https://twitter.com/nickstanch/status/840886207064203264) -- benchmarks below have been updated to include a `Queue` based implementation).

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

`VectorF1` seems perfect. It has stack safe `compose` and `andThen`. Unfortunately, it is terribly slow. Most composed functions are going to be made up of a small number of compositions -- 2, 3, or 4 functions. It's uncommon to compose 1,000,000 functions. `Vector` has effectively constant time cons and snoc, but there are large constant factors. For example, a single element vector will allocate a 32-element array internally. These constant factors end up dominating the performance results, so this solution won't work.

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
  @Benchmark def unitary_QueueF1 = QueueF1(f)(0)

  @Benchmark def compose2_Function1 = (f compose f)(0)
  @Benchmark def compose2_ListF1 = (ListF1(f) compose f)(0)
  @Benchmark def compose2_VectorF1 = (VectorF1(f) compose f)(0)
  @Benchmark def compose2_CatenableF1 = (CatenableF1(f) compose f)(0)
  @Benchmark def compose2_QueueF1 = (QueueF1(f) compose f)(0)

  @Benchmark def andThen2_Function1 = (f andThen f)(0)
  @Benchmark def andThen2_ListF1 = (ListF1(f) andThen f)(0)
  @Benchmark def andThen2_VectorF1 = (VectorF1(f) andThen f)(0)
  @Benchmark def andThen2_CatenableF1 = (CatenableF1(f) andThen f)(0)
  @Benchmark def andThen2_QueueF1 = (QueueF1(f) andThen f)(0)

  @Benchmark def compose1k_Function1 = (0 until 1000).foldLeft(f)((acc, _) => acc compose f)(0)
  @Benchmark def compose1k_ListF1 = (0 until 1000).foldLeft(ListF1(f))((acc, _) => acc compose f)(0)
  @Benchmark def compose1k_VectorF1 = (0 until 1000).foldLeft(VectorF1(f))((acc, _) => acc compose f)(0)
  @Benchmark def compose1k_CatenableF1 = (0 until 1000).foldLeft(CatenableF1(f))((acc, _) => acc compose f)(0)
  @Benchmark def compose1k_QueueF1 = (0 until 1000).foldLeft(QueueF1(f))((acc, _) => acc compose f)(0)

  @Benchmark def andThen1k_Function1 = (0 until 1000).foldLeft(f)((acc, _) => acc andThen f)(0)
  @Benchmark def andThen1k_ListF1 = (0 until 1000).foldLeft(ListF1(f): Int => Int)((acc, _) => acc andThen f)(0)
  @Benchmark def andThen1k_VectorF1 = (0 until 1000).foldLeft(VectorF1(f))((acc, _) => acc andThen f)(0)
  @Benchmark def andThen1k_CatenableF1 = (0 until 1000).foldLeft(CatenableF1(f))((acc, _) => acc andThen f)(0)
  @Benchmark def andThen1k_QueueF1 = (0 until 1000).foldLeft(QueueF1(f))((acc, _) => acc andThen f)(0)

  @Benchmark def interleaved1k_Function1 = (0 until 1000).foldLeft(f)((acc, i) => if (i % 2 == 0) acc compose f else acc andThen f)(0)
  @Benchmark def interleaved1k_ListF1 = (0 until 1000).foldLeft(ListF1(f): Int => Int)((acc, i) => if (i % 2 == 0) acc compose f else acc andThen f)(0)
  @Benchmark def interleaved1k_VectorF1 = (0 until 1000).foldLeft(VectorF1(f))((acc, i) => if (i % 2 == 0) acc compose f else acc andThen f)(0)
  @Benchmark def interleaved1k_CatenableF1 = (0 until 1000).foldLeft(CatenableF1(f))((acc, i) => if (i % 2 == 0) acc compose f else acc andThen f)(0)
  @Benchmark def interleaved1k_QueueF1 = (0 until 1000).foldLeft(QueueF1(f))((acc, i) => if (i % 2 == 0) acc compose f else acc andThen f)(0)
}
```

Running these tests via `jmh:run -i 20 -wi 10 -f1 -t2` results in:

```
[info] Benchmark                                     Mode  Cnt          Score          Error  Units
[info] FunctionBenchmark.andThen1k_CatenableF1      thrpt   20      45571.159 ±     2335.498  ops/s
[info] FunctionBenchmark.andThen1k_Function1        thrpt   20     126595.146 ±     9753.267  ops/s
[info] FunctionBenchmark.andThen1k_ListF1           thrpt   20     122326.955 ±    10781.655  ops/s
[info] FunctionBenchmark.andThen1k_QueueF1          thrpt   20      37697.152 ±     3168.007  ops/s
[info] FunctionBenchmark.andThen1k_VectorF1         thrpt   20      45148.023 ±      442.307  ops/s
[info] FunctionBenchmark.andThen2_CatenableF1       thrpt   20   32498316.581 ±   785428.217  ops/s
[info] FunctionBenchmark.andThen2_Function1         thrpt   20  797337943.963 ± 21834869.821  ops/s
[info] FunctionBenchmark.andThen2_ListF1            thrpt   20  155599638.602 ±  4425324.044  ops/s
[info] FunctionBenchmark.andThen2_QueueF1           thrpt   20   14540756.592 ±   123320.145  ops/s
[info] FunctionBenchmark.andThen2_VectorF1          thrpt   20   14688247.077 ±   131961.840  ops/s
[info] FunctionBenchmark.compose1k_CatenableF1      thrpt   20      69420.052 ±     1235.404  ops/s
[info] FunctionBenchmark.compose1k_Function1        thrpt   20     152949.881 ±     1692.388  ops/s
[info] FunctionBenchmark.compose1k_ListF1           thrpt   20      71636.332 ±     1137.961  ops/s
[info] FunctionBenchmark.compose1k_QueueF1          thrpt   20      51920.074 ±      428.416  ops/s
[info] FunctionBenchmark.compose1k_VectorF1         thrpt   20      40058.457 ±     4437.660  ops/s
[info] FunctionBenchmark.compose2_CatenableF1       thrpt   20   27988815.099 ±  2636173.720  ops/s
[info] FunctionBenchmark.compose2_Function1         thrpt   20  789010032.967 ± 28815277.968  ops/s
[info] FunctionBenchmark.compose2_ListF1            thrpt   20   95572290.279 ±  1972247.968  ops/s
[info] FunctionBenchmark.compose2_QueueF1           thrpt   20   20448278.219 ±   224039.432  ops/s
[info] FunctionBenchmark.compose2_VectorF1          thrpt   20   12204863.300 ±   155372.662  ops/s
[info] FunctionBenchmark.interleaved1k_CatenableF1  thrpt   20      47635.076 ±      556.602  ops/s
[info] FunctionBenchmark.interleaved1k_Function1    thrpt   20     129433.210 ±     1434.155  ops/s
[info] FunctionBenchmark.interleaved1k_ListF1       thrpt   20     134112.681 ±     2443.776  ops/s
[info] FunctionBenchmark.interleaved1k_QueueF1      thrpt   20      43697.844 ±     1693.528  ops/s
[info] FunctionBenchmark.interleaved1k_VectorF1     thrpt   20      30688.994 ±      512.420  ops/s
[info] FunctionBenchmark.unitary_CatenableF1        thrpt   20   48579687.093 ±  1281930.212  ops/s
[info] FunctionBenchmark.unitary_Function1          thrpt   20  809718632.059 ±  6557132.444  ops/s
[info] FunctionBenchmark.unitary_ListF1             thrpt   20  172189813.428 ±  3269454.794  ops/s
[info] FunctionBenchmark.unitary_QueueF1            thrpt   20   22289854.985 ±   500526.148  ops/s
[info] FunctionBenchmark.unitary_VectorF1           thrpt   20   18913195.601 ±   502848.724  ops/s
```

Based on these tests, `CatenableF1` is about 3x faster than `VectorF1` for small compositions and about 1.5x - 2x `VectorF1` for large collections. `CatenableF1` is about 3x slower than `ListF1` and about 14x slower than `Function1` for small compositions and about equal to `ListF1` and 3x slower than `Function1` for large compositions. `CatenableF1` beats `QueueF1` which beats `VectorF1`.

Not bad but stack safety comes at a runtime performance cost. One area for investigation is a dynamic algorithm that switches from `Function1` to `CatenableF1` when the composition stack reaches a certain depth.

## Conclusion

If you check the FS2 source, you might notice that `Free` does not do map fusion. We ended up not merging map fusion in to FS2 because we were able to get comparable performance gains with a simpler optimization that covers more use cases. We now eagerly evaluate `Bind(Pure(a), f)` structures when stepping a `Free`, which gives us the same performance boost as map fusion but covers more use cases and avoids the (minor) constant factor performance hit of `CatenableF1`.
