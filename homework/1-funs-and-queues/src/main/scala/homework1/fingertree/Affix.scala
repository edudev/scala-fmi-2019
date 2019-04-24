package homework1.fingertree

sealed trait Affix[A] {
  def prepend(x: A): Affix[A]
  def append(x: A): Affix[A]
}

object Affix {
  def apply[A](e0: A) = One(e0)
  def apply[A](e0: A, e1: A) = Two(e0, e1)
  def apply[A](e0: A, e1: A, e2: A) = Three(e0, e1, e2)
  def apply[A](e0: A, e1: A, e2: A, e3: A) = Four(e0, e1, e2, e3)
}

case class One[A] (e0: A) extends Affix[A] {
  def prepend(x: A) = Two(x, e0)
  def append(x: A) = Two(e0, x)
}
case class Two[A] (e0: A, e1: A) extends Affix[A] {
  def prepend(x: A) = Three(x, e0, e1)
  def append(x: A) = Three(e0, e1, x)
}
case class Three[A] (e0: A, e1: A, e2: A) extends Affix[A] {
  def prepend(x: A) = Four(x, e0, e1, e2)
  def append(x: A) = Four(e0, e1, e2, x)
}
case class Four[A] (e0: A, e1: A, e2: A, e3: A) extends Affix[A] {
  def prepend(x: A) = throw new UnsupportedOperationException
  def append(x: A) = throw new UnsupportedOperationException
}
