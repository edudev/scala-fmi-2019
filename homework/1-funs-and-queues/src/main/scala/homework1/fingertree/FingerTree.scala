package homework1.fingertree

sealed trait FingerTree[A] {
  def prepend(x: A): FingerTree[A]
  def append(x: A): FingerTree[A]

  def viewl: ViewT[A]
  def viewr: ViewT[A]

  def head = viewl match {
    case EmptyView() => throw new NoSuchElementException
    case View(x, _) => x
  }
  def tail = viewl match {
    case EmptyView() => throw new UnsupportedOperationException
    case View(_, rest) => rest
  }

  def last = viewr match {
    case EmptyView() => throw new NoSuchElementException
    case View(x, _) => x
  }
  def init = viewr match {
    case EmptyView() => throw new UnsupportedOperationException
    case View(_, rest) => rest
  }
}

object FingerTree {
  def empty[A]: FingerTree[A] = Empty[A]()
}

case class Empty[A] () extends FingerTree[A] {
  def prepend(x: A) = Single(x)
  def append(x: A) = Single(x)

  def viewl = EmptyView()
  def viewr = EmptyView()
}

case class Single[A] (root: A) extends FingerTree[A] {
  def prepend(x: A) = Deep(Affix(x), Empty(), Affix(root))
  def append(x: A) = Deep(Affix(root), Empty(), Affix(x))

  def viewl = View(root, Empty())
  def viewr = View(root, Empty())
}

case class Deep[A] (prefix: Affix[A], deeper: FingerTree[Node[A]], suffix: Affix[A]) extends FingerTree[A] {
  def prepend(x: A) = prefix match {
    case Four(e0, e1, e2, e3) => Deep(Affix(x, e0), deeper.prepend(Branch3(e1, e2, e3)), suffix)
    case _ => Deep(prefix.prepend(x), deeper, suffix)
  }
  def append(x: A) = suffix match {
    case Four(e0, e1, e2, e3) => Deep(prefix, deeper.append(Branch3(e0, e1, e2)), Affix(e3, x))
    case _ => Deep(prefix, deeper, suffix.append(x))
  }

  def viewl = prefix match {
    case One(e0) => {
      val subtree = deeper.viewl match {
        case View(e1, rest) => Deep(e1.toAffix, rest, suffix)
        case EmptyView() => suffix match {
          case One(e1) => Single(e1)
          case Two(e1, e2) => Deep(Affix(e1), Empty[Node[A]](), Affix(e2))
          case Three(e1, e2, e3) => Deep(Affix(e1, e2), Empty[Node[A]](), Affix(e3))
          case Four(e1, e2, e3, e4) => Deep(Affix(e1, e2, e3), Empty[Node[A]](), Affix(e4))
        }
      }

      View(e0, subtree)
    }
    case Two(e0, e1) => View(e0, Deep(Affix(e1), deeper, suffix))
    case Three(e0, e1, e2) => View(e0, Deep(Affix(e1, e2), deeper, suffix))
    case Four(e0, e1, e2, e3) => View(e0, Deep(Affix(e1, e2, e3), deeper, suffix))
  }

  def viewr = suffix match {
    case One(e0) => {
      // I have no idea why it doesn't compile if I leave out the type
      val subtree: FingerTree[A] = deeper.viewr match {
        case View(e1, rest) => Deep(prefix, rest, e1.toAffix)
        case EmptyView() => prefix match {
          case One(e1) => Single(e1)
          case Two(e1, e2) => Deep(Affix(e1), Empty[Node[A]](), Affix(e2))
          case Three(e1, e2, e3) => Deep(Affix(e1), Empty[Node[A]](), Affix(e2, e3))
          case Four(e1, e2, e3, e4) => Deep(Affix(e1), Empty[Node[A]](), Affix(e2, e3, e4))
        }
      }

      View(e0, subtree)
    }
    case Two(e0, e1) => View(e1, Deep(prefix, deeper, Affix(e0)))
    case Three(e0, e1, e2) => View(e2, Deep(prefix, deeper, Affix(e0, e1)))
    case Four(e0, e1, e2, e3) => View(e3, Deep(prefix, deeper, Affix(e0, e1, e2)))
  }
}
