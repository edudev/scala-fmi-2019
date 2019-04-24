package homework1.fingertree

sealed trait Node[A] {
  def toAffix: Affix[A]
}

case class Branch3[A] (e0: A, e1: A, e2: A) extends Node[A] {
  def toAffix = Affix(e0, e1, e2)
}

case class Branch2[A] (e0: A, e1: A) extends Node[A] {
  def toAffix = Affix(e0, e1)
}
