package homework2

sealed trait Validated[+E, +A] {
  def isValid: Boolean = this match {
    case Valid(_)   => true
    case Invalid(_) => false
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Valid(a) => a
    case _        => default
  }

  def orElse[F >: E, B >: A](default: => Validated[F, B]): Validated[F, B] =
    if (isValid) this else default

  def zip[EE >: E, B](vb: Validated[EE, B]): Validated[EE, (A, B)] =
    (this, vb) match {
      case (Valid(a), Valid(b))      => Valid((a, b))
      case (Invalid(a), Invalid(b))  => Invalid(a ++ b)
      case (invalid: Invalid[EE], _) => invalid
      case (_, invalid: Invalid[EE]) => invalid
    }

  def map[B](f: A => B): Validated[E, B] = this match {
    case Valid(a)            => Valid(f(a))
    case invalid: Invalid[E] => invalid
  }

  def map2[EE >: E, B, R](
      vb: Validated[EE, B]
  )(f: (A, B) => R): Validated[EE, R] =
    zip(vb).map(f.tupled)

  def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] =
    this match {
      case Valid(a)            => f(a)
      case invalid: Invalid[E] => invalid
    }

  def fold[B](invalid: Chain[E] => B, valid: A => B): B = this match {
    case Invalid(errors) => invalid(errors)
    case Valid(a)        => valid(a)
  }

  def foreach(f: A => Unit): Unit = fold(_ => (), f)
}

case class Valid[+A](a: A)               extends Validated[Nothing, A]
case class Invalid[+E](errors: Chain[E]) extends Validated[E, Nothing]

object Invalid {
  def apply[E](error: E): Invalid[E] = Invalid(Chain(error))
}

object Validated {
  def sequence[E, A](xs: List[Validated[E, A]]): Validated[E, List[A]] =
    xs.foldRight(Valid(List.empty[A]): Validated[E, List[A]])(
      _.map2(_)(_ :: _)
    )

  implicit class ValidatedTuple2[EE, A, B](
      val tuple: (Validated[EE, A], Validated[EE, B])
  ) extends AnyVal {
    def zip: Validated[EE, (A, B)]                  = tuple._1 zip tuple._2
    def zipMap[R](f: (A, B) => R): Validated[EE, R] = tuple.zip.map(f.tupled)
  }

  implicit class ValidatedTuple3[EE, A, B, C](
      val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C])
  ) extends AnyVal {
    def zip: Validated[EE, (A, B, C)] =
      (tuple._1, tuple._2).zip.map2(tuple._3)((ab, c) => (ab._1, ab._2, c))
    def zipMap[R](f: (A, B, C) => R): Validated[EE, R] =
      tuple.zip.map(f.tupled)
  }

  implicit class ValidatedTuple4[EE, A, B, C, D](
      val tuple: (
          Validated[EE, A],
          Validated[EE, B],
          Validated[EE, C],
          Validated[EE, D]
      )
  ) extends AnyVal {
    def zip: Validated[EE, (A, B, C, D)] =
      (tuple._1, tuple._2, tuple._3).zip.map2(tuple._4)(
        (abc, d) => (abc._1, abc._2, abc._3, d)
      )
    def zipMap[R](f: (A, B, C, D) => R): Validated[EE, R] =
      tuple.zip.map(f.tupled)
  }

  implicit class ValidatedTuple5[EE, A, B, C, D, E](
      val tuple: (
          Validated[EE, A],
          Validated[EE, B],
          Validated[EE, C],
          Validated[EE, D],
          Validated[EE, E]
      )
  ) extends AnyVal {
    def zip: Validated[EE, (A, B, C, D, E)] =
      (tuple._1, tuple._2, tuple._3, tuple._4).zip.map2(tuple._5)(
        (abcd, e) => (abcd._1, abcd._2, abcd._3, abcd._4, e)
      )
    def zipMap[R](f: (A, B, C, D, E) => R): Validated[EE, R] =
      tuple.zip.map(f.tupled)
  }

  implicit class ValidatedOption[+A](val option: Option[A]) extends AnyVal {
    def toValidated[E](onEmpty: => E): Validated[E, A] =
      option.fold(Invalid(onEmpty): Validated[E, A])(Valid(_))
  }
}
