package homework2

import scala.annotation.tailrec

sealed trait Chain[+A] {
  def head: A
  def tail: Option[Chain[A]]

  def isEmpty: Boolean = false

  def +:[B >: A](front: B): Chain[B] = Append(Singleton(front), this)

  def :+[B >: A](back: B): Chain[B] = Append(this, Singleton(back))

  def ++[B >: A](right: Chain[B]): Chain[B] = Append(this, right)

  /*
  def foldLeft[B](initial: B)(f: (B, A) => B): B = this match {
    case Singleton(element) => f(initial, element)
    case Append(left, right) => right.foldLeft(left.foldLeft(initial)(f))(f)
  }
   */

  def foldLeft[B](initial: B)(f: (B, A) => B): B = {
    @tailrec
    def helper(l: List[Chain[A]], r: List[Chain[A]], acc: B): B =
      l match {
        case Singleton(element) :: ls  => helper(ls, r, f(acc, element))
        case Append(left, right) :: ls => helper(left :: ls, right :: r, acc)
        case Nil =>
          r match {
            case Singleton(element) :: rs => helper(l, rs, f(acc, element))
            case Append(left, right) :: rs =>
              helper(left :: l, right :: rs, acc)
            case Nil => acc
          }
      }

    helper(List(this), Nil, initial)
  }

  /*
  def foldRight[B](initial: B)(f: (A, B) => B): B = this match {
    case Singleton(element) => f(element, initial)
    case Append(left, right) => left.foldRight(right.foldRight(initial)(f))(f)
  }
   */

  def reduceLeft[B >: A](f: (B, A) => B): B =
    foldLeft(Option.empty[B])(
      (acc, element) => Some(acc.fold(element: B)(f(_, element)))
    ).get // safe, as there is always at least one element

  /*
  def reduceLeft[B >: A](f: (B, A) => B): B = this.listify match {
    case Singleton(first) => first
    case Append(Singleton(first), rest) => rest.foldLeft(first: B)(f)
    case _ => sys.error("Unexpected listify format")
  }
   */

  /*
  def map[B](f: A => B): Chain[B] = this match {
    case Singleton(element) => Singleton(f(element))
    case Append(left, right) => Append(left map f, right map f)
  }
   */

  def map[B](f: A => B): Chain[B] =
    foldLeft(Option.empty[Chain[B]])(
      (acc, element) =>
        Some(acc.fold(Singleton(f(element)): Chain[B])(_ :+ f(element)))
    ).get // safe, as there is always at least one element

  /*
  def flatMap[B](f: A => Chain[B]): Chain[B] = this match {
    case Singleton(element) => f(element)
    case Append(left, right) => Append(left flatMap f, right flatMap f)
  }
   */

  def flatMap[B](f: A => Chain[B]): Chain[B] =
    foldLeft(Option.empty[Chain[B]])(
      (acc, element) => Some(acc.fold(f(element))(_ ++ f(element)))
    ).get // safe, as there is always at least one element

  def foreach(f: A => Unit): Unit = foldLeft(())((_, next) => f(next))

  /*
  override def equals(that: Any): Boolean = that match {
    case c: Chain[_] => toList == c.toList
    case _ => false
  }
   */

  override def equals(that: Any): Boolean = {
    @tailrec
    def helper[B](one: Chain[A], two: Chain[B]): Boolean = (one, two) match {
      case (Singleton(a), Singleton(b)) => a == b
      case (Append(Singleton(a), restA), Append(Singleton(b), restB)) =>
        a == b && helper(restA, restB)
      case _ => false
    }

    that match {
      case c: Chain[_] => helper(this.listify, c.listify)
      case _           => false
    }
  }

  override def hashCode: Int = foldLeft(0) {
    _ * 31 + _.hashCode
  }

  override def toString: String = toList.mkString("Chain(", ",", ")")

  def toList: List[A] =
    foldLeft(List.empty[A])((acc, next) => next :: acc).reverse
  def toSet[B >: A]: Set[B] = foldLeft(Set.empty[B])((acc, next) => acc + next)

  def min[B >: A](implicit order: Ordering[B]): A =
    reduceLeft(order.min).asInstanceOf[A]
  def max[B >: A](implicit order: Ordering[B]): A =
    reduceLeft(order.max).asInstanceOf[A]

  /*
  def listify: Chain[A] = foldRight(Option.empty[Chain[A]])((element, acc) =>
    Some(acc.fold(Singleton(element): Chain[A])(Append(Singleton(element), _)))
  ).get // safe, as there is always at least one element
   */

  def listify: Chain[A] = {
    @tailrec
    def initial_helper(r: List[Chain[A]], l: List[Chain[A]]): Chain[A] =
      r match {
        case (last @ Singleton(_)) :: rs => helper(rs, l, last)
        case Append(left, right) :: rs   => initial_helper(right :: rs, left :: l)
        case Nil                         => sys.error("Unexpected format")
      }

    @tailrec
    def helper(r: List[Chain[A]], l: List[Chain[A]], acc: Chain[A]): Chain[A] =
      r match {
        case Singleton(element) :: rs  => helper(rs, l, element +: acc)
        case Append(left, right) :: rs => helper(right :: rs, left :: l, acc)
        case Nil =>
          l match {
            case Singleton(element) :: ls => helper(r, ls, element +: acc)
            case Append(left, right) :: ls =>
              helper(right :: r, left :: ls, acc)
            case Nil => acc
          }
      }

    initial_helper(List(this), Nil)
  }
}

case class Singleton[+A](head: A) extends Chain[A] {
  def tail: Option[Chain[A]] = None
}
case class Append[+A](left: Chain[A], right: Chain[A]) extends Chain[A] {
  def head: A = left.head
  def tail: Option[Chain[A]] = left match {
    case Singleton(_) => Some(right)
    case _            => listify.tail
  }
}

object Chain {
  def apply[A](head: A, rest: A*): Chain[A] =
    if (rest.isEmpty) Singleton(head)
    else Append(Singleton(head), Chain(rest.head, rest.tail: _*))

  // Allows Chain to be used in pattern matching
  def unapplySeq[A](chain: Chain[A]): Option[Seq[A]] = Some(chain.toList)
}
