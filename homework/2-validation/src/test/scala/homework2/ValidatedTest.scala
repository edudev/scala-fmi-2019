package homework2

import org.scalatest.{FlatSpec, FunSuite, Matchers}

class ValidatedTest extends FlatSpec with Matchers {
  "isValid of Valid" should "be always true" in {
    Valid(1).isValid shouldEqual true
    Valid(false).isValid shouldEqual true
  }

  "isValid of Invalid" should "be always false" in {
    Invalid("").isValid shouldEqual false
    Invalid(true).isValid shouldEqual false
  }

  "getOrElse of Valid" should "be non-strict" in {
    Valid(42).getOrElse(Stream.from(1).toList) shouldEqual 42
  }

  "getOrElse of Invalid" should "return the default value" in {
    Invalid(42).getOrElse(24) shouldEqual 24
  }

  "orElse of Valid" should "be non-strict" in {
    Valid(42).orElse(Valid(Stream.from(1).toList)) shouldEqual Valid(42)
    Valid(42).orElse(Invalid(Stream.from(1).toList)) shouldEqual Valid(42)
  }

  "orElse of Invalid" should "return the default value" in {
    Invalid(42).orElse(Valid(24)) shouldEqual Valid(24)
    Invalid(42).orElse(Invalid(24)) shouldEqual Invalid(24)
  }

  "zip" should "combine valid instances" in {
    Valid(1).zip(Valid("a")) shouldEqual Valid((1, "a"))
  }

  it should "combine errors from invalid instances" in {
    Invalid(1).zip(Invalid(Chain(2, 3))) shouldEqual Invalid(Chain(1, 2, 3))
  }

  it should "combine errors from at least one invalid instance" in {
    Valid(1).zip(Invalid(Chain(2, 3))) shouldEqual Invalid(Chain(2, 3))
    Invalid(1).zip(Valid(23)) shouldEqual Invalid(1)
  }

  "map of Valid" should "transform the value" in {
    Valid(42).map(_ => 24) shouldEqual Valid(24)
  }

  "map of Invalid" should "not change the error" in {
    Invalid(42).map(_ => 24) shouldEqual Invalid(42)
  }

  "map2" should "combine valid instances and transform them" in {
    Valid(-5).map2(Valid(42))(_ + _) shouldEqual Valid(37)
  }

  it should "combine errors from invalid instances" in {
    Invalid(1)
      .map2(Invalid(Chain(2, 3)))((_, _) => 24)
      .shouldEqual(Invalid(Chain(1, 2, 3)))
  }

  it should "combine errors from at least one invalid instance" in {
    Valid(1)
      .map2(Invalid(Chain(2, 3)))((_, _) => 24)
      .shouldEqual(Invalid(Chain(2, 3)))

    Invalid(1).map2(Valid(23))((_, _) => 24) shouldEqual Invalid(1)
  }

  "flatMap of Valid" should "transform the value" in {
    Valid(42).flatMap(_ => Valid(24)) shouldEqual Valid(24)
    Valid(42).flatMap(_ => Invalid(24)) shouldEqual Invalid(24)
  }

  "flatMap of Invalid" should "preserve the instance" in {
    Invalid(42).flatMap(_ => Valid(24)) shouldEqual Invalid(42)
    Invalid(42).flatMap(_ => Invalid(24)) shouldEqual Invalid(42)
  }

  "fold of Valid" should "transform the value" in {
    Valid(42).fold(_ => Stream.from(1).toList, _ * 2) shouldEqual 84
  }

  "fold of Invalid" should "transform the errors" in {
    Invalid(42)
      .fold(_.toList, _ => Stream.from(1).toList)
      .shouldEqual(List(42))
  }

  "foreach of Valid" should "call the function" in {
    val buffer = scala.collection.mutable.ListBuffer.empty[Int]
    Valid(42).foreach(buffer append _)
    buffer.toList shouldEqual List(42)
  }

  "foreach of Invalid" should "not call anything" in {
    val buffer = scala.collection.mutable.ListBuffer.empty[Int]
    Invalid(42).foreach(buffer append _)
    buffer.toList shouldEqual List()
  }

  "Tuple2 zip" should "combine valid instances" in {
    (Valid(1), Valid("a")).zip shouldBe Valid((1, "a"))
  }

  it should "combine errors from invalid instances" in {
    (Invalid(1.0), Invalid(Chain(2, 3))).zip shouldBe Invalid(Chain(1.0, 2, 3))
  }

  it should "combine errors from at least one invalid instance" in {
    (Valid(1), Invalid(Chain(2, 3))).zip shouldBe Invalid(Chain(2, 3))
    (Invalid(1), Valid(23)).zip shouldBe Invalid(1)
  }

  "Tuple2 zipMap" should "combine valid instances and transform them" in {
    (Valid(-5), Valid(42)).zipMap(_ + _) shouldBe Valid(37)
  }

  it should "combine errors from invalid instances" in {
    (Invalid(1), Invalid(Chain(2, 3)))
      .zipMap((_: Nothing, _: Nothing) => 24)
      .shouldBe(Invalid(Chain(1, 2, 3)))
  }

  it should "combine errors from at least one invalid instance" in {
    (Valid(1), Invalid(Chain(2, 3)))
      .zipMap((_, _: Nothing) => 24)
      .shouldBe(Invalid(Chain(2, 3)))

    (Invalid(1), Valid(23)).zipMap((_: Nothing, _) => 24) shouldBe Invalid(1)
  }

  "Tuple3 zip" should "combine valid instances" in {
    (Valid(1), Valid(2), Valid(3)).zip shouldBe Valid((1, 2, 3))
  }

  it should "combine errors from invalid instances" in {
    (Invalid(1), Invalid(2), Invalid(3)).zip shouldBe Invalid(Chain(1, 2, 3))
  }

  it should "combine errors from at least one invalid instance" in {
    (Valid(1), Invalid(2), Valid(3)).zip shouldBe Invalid(2)
    (Invalid(1), Valid(2), Invalid(3)).zip shouldBe Invalid(Chain(1, 3))
  }

  "Tuple3 zipMap" should "combine valid instances and transform them" in {
    (Valid(1), Valid(2), Valid(3)).zipMap(_ + _ + _) shouldBe Valid(6)
  }

  it should "combine errors from invalid instances" in {
    (Invalid(1), Invalid(2), Invalid(3))
      .zipMap((_: Nothing, _: Nothing, _: Nothing) => 24)
      .shouldBe(Invalid(Chain(1, 2, 3)))
  }

  it should "combine errors from at least one invalid instance" in {
    (Valid(1), Invalid(2), Valid(3))
      .zipMap((_, _: Nothing, _) => 24)
      .shouldBe(Invalid(2))

    (Invalid(1), Valid(2), Invalid(3))
      .zipMap((_: Nothing, _, _: Nothing) => 42)
      .shouldBe(Invalid(Chain(1, 3)))
  }

  "Tuple4 zip" should "combine valid instances" in {
    (Valid(1), Valid(2), Valid(3), Valid(4)).zip shouldBe Valid((1, 2, 3, 4))
  }

  it should "combine errors from invalid instances" in {
    (Invalid(1), Invalid(2), Invalid(3), Invalid(4)).zip
      .shouldBe(Invalid(Chain(1, 2, 3, 4)))
  }

  it should "combine errors from at least one invalid instance" in {
    (Valid(1), Invalid(2), Valid(3), Valid(4)).zip shouldBe Invalid(2)
    (Invalid(1), Valid(2), Invalid(3), Invalid(4)).zip
      .shouldBe(Invalid(Chain(1, 3, 4)))
  }

  "Tuple4 zipMap" should "combine valid instances and transform them" in {
    (Valid(1), Valid(2), Valid(3), Valid(4))
      .zipMap(_ + _ + _ + _)
      .shouldBe(Valid(10))
  }

  it should "combine errors from invalid instances" in {
    (Invalid(1), Invalid(2), Invalid(3), Invalid(4))
      .zipMap((_: Nothing, _: Nothing, _: Nothing, _: Nothing) => 24)
      .shouldBe(Invalid(Chain(1, 2, 3, 4)))
  }

  it should "combine errors from at least one invalid instance" in {
    (Valid(1), Invalid(2), Valid(3), Valid(4))
      .zipMap((_, _: Nothing, _, _) => 24)
      .shouldBe(Invalid(2))

    (Invalid(1), Valid(2), Invalid(3), Invalid(4))
      .zipMap((_: Nothing, _, _: Nothing, _: Nothing) => 42)
      .shouldBe(Invalid(Chain(1, 3, 4)))
  }

  "Tuple5 zip" should "combine valid instances" in {
    (Valid(1), Valid(2), Valid(3), Valid(4), Valid(5)).zip
      .shouldBe(Valid((1, 2, 3, 4, 5)))
  }

  it should "combine errors from invalid instances" in {
    (Invalid(1), Invalid(2), Invalid(3), Invalid(4), Invalid(5)).zip
      .shouldBe(Invalid(Chain(1, 2, 3, 4, 5)))
  }

  it should "combine errors from at least one invalid instance" in {
    (Valid(1), Invalid(2), Valid(3), Valid(4), Valid(5)).zip
      .shouldBe(Invalid(2))
    (Invalid(1), Valid(2), Invalid(3), Invalid(4), Invalid(5)).zip
      .shouldBe(Invalid(Chain(1, 3, 4, 5)))
  }

  "Tuple5 zipMap" should "combine valid instances and transform them" in {
    (Valid(1), Valid(2), Valid(3), Valid(4), Valid(5))
      .zipMap(_ + _ + _ + _ + _)
      .shouldBe(Valid(15))
  }

  it should "combine errors from invalid instances" in {
    (Invalid(1), Invalid(2), Invalid(3), Invalid(4), Invalid(5))
      .zipMap(
        (_: Nothing, _: Nothing, _: Nothing, _: Nothing, _: Nothing) => 24
      )
      .shouldBe(Invalid(Chain(1, 2, 3, 4, 5)))
  }

  it should "combine errors from at least one invalid instance" in {
    (Valid(1), Invalid(2), Valid(3), Valid(4), Valid(5))
      .zipMap((_, _: Nothing, _, _, _) => 24)
      .shouldBe(Invalid(2))

    (Invalid(1), Valid(2), Invalid(3), Invalid(4), Invalid(5))
      .zipMap((_: Nothing, _, _: Nothing, _: Nothing, _: Nothing) => 42)
      .shouldBe(Invalid(Chain(1, 3, 4, 5)))
  }

  "sequence" should "combine valid instances" in {
    val range = 1 to 20
    Validated.sequence(range.map(Valid(_)).toList) shouldBe Valid(range.toList)
  }

  it should "combine errors from invalid instances" in {
    val range = 1 to 20
    Validated
      .sequence(range.map(Invalid(_)).toList)
      .shouldBe(Invalid(Chain(range.head, range.tail: _*)))
  }

  it should "combine errors from at least one invalid instance" in {
    val source1 = (1 to 20)
      .map(Valid(_))
      .toList
      .updated(1, Invalid(2))
      .updated(13, Invalid(41))

    Validated.sequence(source1) shouldBe Invalid(Chain(2, 41))

    val source2 = (1 to 20)
      .map(Invalid(_))
      .toList
      .updated(1, Valid(2))
      .updated(13, Valid(41))

    Validated
      .sequence(source2)
      .shouldBe(
        Invalid(
          Chain(1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20)
        )
      )
  }

  "toValidated of None" should "return the invalid value" in {
    Option.empty[Valid[Int]].toValidated(42) shouldEqual Invalid(42)
  }

  "toValidated of Some" should "return the option valid as valid" in {
    Some(Invalid(1))
      .toValidated(Stream.from(1).toList)
      .shouldEqual(Valid(Invalid(1)))
  }
}
