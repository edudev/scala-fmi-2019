package homework1

import scala.annotation.tailrec
import scala.collection.immutable.Set


object Functions {
  /*
  def fromDigits(digits: List[Int], radix: Int = 10): Int =
    digits.foldLeft(0)(_ * radix + _)
  */


  def fromDigits(digits: List[Int], radix: Int = 10): Int = {
    @tailrec
    def helper(digits: List[Int], radix: Int, acc: Int): Int = digits match {
      case Nil => acc
      case digit :: rest if digit >= 0 && digit <= radix =>
        helper(rest, radix, acc * radix + digit)
    }
    helper(digits, radix, 0)
  }

  /*
  def parseInteger(integer: String, radix: Int = 10): Int = {
    def parseDigit(digit: Char): Int = digit match {
      case digit if digit >= '0' && digit <= '9' => digit - '0'
      case digit if digit >= 'A' && digit <= 'Z' => digit - 'A' + 10
    }

    integer.head match {
      case '-' => -parseInteger(integer.tail, radix)
      case _ => fromDigits(integer.map(parseDigit).toList, radix)
    }
  }
  */

  def parseInteger(integer: String, radix: Int = 10): Int = {
    def check(digit: Int, radix: Int): Int = digit match {
      case digit if digit < radix => digit
    }
    def parseDigit(digit: Char, radix: Int): Int = digit match {
      case digit if digit >= '0' && digit <= '9' => check(digit - '0', radix)
      case digit if digit >= 'A' && digit <= 'Z' => check(digit - 'A' + 10, radix)
    }

    @tailrec
    def helper(integer: String, radix: Int, acc: Int): Int = integer match {
      case integer if integer.isEmpty => acc
      case _ => helper(integer.tail, radix, acc * radix + parseDigit(integer.head, radix))
    }

    def parsePositiveInteger(integer: String, radix: Int, acc: Int): Int = integer.length match {
      case length if length > 0 => helper(integer, radix, 0)
    }

    integer.head match {
      case '-' => -parsePositiveInteger(integer.tail, radix, 0)
      case _ => parsePositiveInteger(integer, radix, 0)
    }
  }


  def zipMap(a: List[Int], b: List[Int], f: (Int, Int) => Int): List[Int] = {
    @tailrec
    def helper(a: List[Int], b: List[Int], f: (Int, Int) => Int, acc: List[Int]): List[Int] = a.length * b.length match {
      case 0 => acc.reverse
      case _ => helper(a.tail, b.tail, f, f(a.head, b.head) :: acc)
    }

    helper(a, b, f, Nil)
  }

  def countCoinChangeVariants(denominations: List[Int], change: Int): Int = (denominations, change) match {
    case (_, 0) => 1
    case (Nil, _) => 0
    case (denomination :: rest, change) if change < denomination =>
      countCoinChangeVariants(rest, change)
    case (denomination :: rest, change) =>
      countCoinChangeVariants(rest, change) +
      countCoinChangeVariants(denominations, change - denomination)
  }

  def bfsTraversal(start: Int, end: Int, neighbours: Int => List[Int]): Queue[Int] = {
    @tailrec
    def visitNodes(visited: Set[Int], toVisit: Queue[Int], acc: Queue[Int], newNeighbours: List[Int]): Queue[Int] =
      if (newNeighbours.isEmpty)
        helper(visited, toVisit, acc)
      else {
        val node = newNeighbours.head
        if (node == end)
          acc.push(end)
        else if (visited.contains(node))
          visitNodes(visited, toVisit, acc, newNeighbours.tail)
        else
          visitNodes(visited + node, toVisit.push(node), acc.push(node), newNeighbours.tail)
      }

    def helper(visited: Set[Int], toVisit: Queue[Int], acc: Queue[Int]): Queue[Int] =
      if (toVisit.isEmpty)
        acc
      else {
        val node = toVisit.peek
        visitNodes(visited, toVisit.pop, acc, neighbours(node))
      }

    helper(Set(start), Queue(List(start)), Queue(List(start)))
  }
}
