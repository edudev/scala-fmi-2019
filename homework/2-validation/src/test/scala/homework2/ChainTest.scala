package homework2

import org.scalatest.{FlatSpec, Matchers}

class ChainTest extends FlatSpec with Matchers {
  "Chain apply with one argument" should "create a Singleton chain" in {
    Chain(42) shouldEqual Singleton(42)
  }

  "Chain apply with many arguments" should "create an Append chain" in {
    Chain(1, 2, 3)
      .shouldEqual(Append(Singleton(1), Append(Singleton(2), Singleton(3))))
  }

  "head of Singleton" should "return only one element" in {
    Singleton(42).head shouldEqual 42
  }

  "head of Append" should "return first element" in {
    Append(Chain(1, 2), Chain(11, 12)).head shouldEqual 1
  }

  "tail of Singleton" should "return None" in {
    Singleton(42).tail shouldEqual None
  }

  "tail of Append" should "return remainder of chain" in {
    Append(Chain(1, 2), Chain(11, 12)).tail shouldEqual Some(Chain(2, 11, 12))
  }

  "isEmpty of Singleton" should "return false" in {
    Singleton(42).isEmpty shouldEqual false
  }

  "isEmpty of Append" should "return false" in {
    Append(Chain(1, 2), Chain(11, 12)).isEmpty shouldEqual false
  }

  "+: of Singleton" should "add an element in front" in {
    24 +: Singleton(42) shouldEqual Chain(24, 42)
  }

  "+: of Append" should "add an element in front" in {
    24 +: Append(Chain(1, 2), Chain(11, 12)) shouldEqual
      Chain(24, 1, 2, 11, 12)
  }

  ":+ of Singleton" should "add an element in front" in {
    Singleton(42) :+ 24 shouldEqual Chain(42, 24)
  }

  ":+ of Append" should "add an element in front" in {
    Append(Chain(1, 2), Chain(11, 12)) :+ 24 shouldEqual
      Chain(1, 2, 11, 12, 24)
  }

  "++ of two Singletons" should "append two them" in {
    (Singleton(24) ++ Singleton(42)) shouldEqual Chain(24, 42)
  }

  "++ of two chains" should "append them" in {
    (Chain(1, 2) ++ Chain(3, 4)) shouldEqual Chain(1, 2, 3, 4)
  }

  "foldLeft of Singleton" should "iterate once" in {
    Singleton(42).foldLeft(0)(_ - _) shouldEqual -42
    Singleton(2.0).foldLeft(1.0)(_ / _) shouldEqual 0.5
  }

  "foldLeft of balanced Append" should "iterate properly" in {
    Append(Chain(1, 2), Chain(11, 12)).foldLeft(0)(_ - _) shouldEqual -26
    Append(Chain(2.0, 4.0), Chain(8.0, 16.0))
      .foldLeft(512.0)(_ / _)
      .shouldEqual(0.5)
  }

  "foldLeft of left-weighted Append" should "iterate properly" in {
    Append(Chain(1), Append(Chain(2), Chain(11, 12)))
      .foldLeft(0)(_ - _)
      .shouldEqual(-26)
    Append(Chain(2.0), Append(Chain(4.0), Chain(8.0, 16.0)))
      .foldLeft(512.0)(_ / _) shouldEqual (0.5)
  }

  "foldLeft of right-weighted Append" should "iterate properly" in {
    Append(Append(Chain(1, 2), Chain(11)), Chain(12))
      .foldLeft(0)(_ - _)
      .shouldEqual(-26)
    Append(Append(Chain(2.0, 4.0), Chain(8.0)), Chain(16.0))
      .foldLeft(512.0)(_ / _)
      .shouldEqual(0.5)
  }

  "reduceLeft of Singleton" should "not iterate" in {
    Singleton(42).reduceLeft((_, _) => 0) shouldEqual 42
  }

  "reduceLeft of Append" should "iterate properly" in {
    Append(Chain(1, 2), Chain(11, 12)).reduceLeft(_ + _) shouldEqual 26
    Append(Chain(1, 2), Chain(11, 12)).reduceLeft(_ * _) shouldEqual 264
  }

  "map of Singleton" should "map properly" in {
    Singleton(42) map (_ * 2) shouldEqual Singleton(84)
  }

  "map of Append" should "map properly" in {
    Append(Chain(1, 2), Chain(11, 12))
      .map(_ * 2)
      .shouldEqual(Append(Chain(2, 4), Chain(22, 24)))
  }

  "flatMap of Singleton" should "flatMap properly" in {
    Singleton(List(4, 2))
      .flatMap(l => Chain(l.head, l.tail: _*))
      .shouldEqual(Chain(4, 2))
  }

  "flatMap of Append" should "flatMap properly" in {
    Append(Chain(List(1, 2), List(11)), Chain(List(21, 22), List(31, 32, 33)))
      .flatMap(l => Chain(l.head, l.tail: _*))
      .shouldEqual(
        Append(
          Append(Chain(1, 2), Chain(11)),
          Append(Chain(21, 22), Chain(31, 32, 33))
        )
      )
  }

  "foreach of Singleton" should "iterate properly" in {
    val buffer = scala.collection.mutable.ListBuffer.empty[Int]
    Singleton(42).foreach(buffer append _)
    buffer.toList shouldEqual List(42)
  }

  "foreach of Append" should "iterate properly" in {
    val buffer = scala.collection.mutable.ListBuffer.empty[Int]
    Append(Chain(1, 2), Chain(11, 12)).foreach(buffer append _)
    buffer.toList shouldEqual List(1, 2, 11, 12)
  }

  "equals of Singleton" should "compare equality" in {
    Singleton(42) == Singleton(42) shouldBe true
    Singleton(42) == Singleton(24) shouldBe false
  }

  "equals of Append" should "compare equality" in {
    (Append(Chain(1, 2), Chain(11, 12)) == Append(Chain(1, 2), Chain(11, 12)))
      .shouldBe(true)
    (Append(Chain(1, 2), Chain(11, 12)) == Append(Chain(10, 2), Chain(11, 12)))
      .shouldBe(false)
  }

  "equals of mixed chains" should "compare equality" in {
    Append(Chain(1, 2), Chain(11, 12)) == Chain(1, 2, 11, 12) shouldBe true
    Singleton(42) == Chain(42) shouldBe true
    Singleton(42) == Append(Chain(1, 2), Chain(11, 12)) shouldBe false
  }

  "toList of Singleton" should "convert properly" in {
    Singleton(42).toList shouldEqual List(42)
  }

  "toList of Append" should "convert properly" in {
    Append(Chain(1, 2), Chain(11, 12)).toList shouldEqual List(1, 2, 11, 12)
  }

  "toSet of Singleton" should "convert properly" in {
    Singleton(42).toSet shouldEqual Set(42)
  }

  "toSet of Append" should "convert properly" in {
    Append(Chain(1, 12), Chain(12, 11, 12)).toSet shouldEqual Set(1, 11, 12)
  }

  "min of Singleton" should "order properly" in {
    Singleton(42).min shouldEqual 42
  }

  "min of Append" should "order properly" in {
    Append(Chain(1, 100, 2), Chain(11, -5, 12)).min shouldEqual -5
  }

  "max of Singleton" should "order properly" in {
    Singleton(42).max shouldEqual 42
  }

  "max of Append" should "order properly" in {
    Append(Chain(1, 100, 2), Chain(11, -5, 12)).max shouldEqual 100
  }
}
