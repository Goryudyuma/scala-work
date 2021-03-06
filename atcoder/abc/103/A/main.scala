import java.util.Scanner

import scala.collection.Searching._
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable._
import scala.io.StdIn.readLine

class IUnionFind(val size: Int) {

  private case class Node(var parent: Option[Int], var treeSize: Int)

  private val nodes = Array.fill[Node](size)(new Node(None, 1))

  def union(t1: Int, t2: Int): IUnionFind = {
    if (t1 == t2) return this

    val root1 = root(t1)
    val root2 = root(t2)
    if (root1 == root2) return this

    val node1 = nodes(root1)
    val node2 = nodes(root2)

    if (node1.treeSize < node2.treeSize) {
      node1.parent = Some(t2)
      node2.treeSize += node1.treeSize
    } else {
      node2.parent = Some(t1)
      node1.treeSize += node2.treeSize
    }
    this
  }

  def connected(t1: Int, t2: Int): Boolean = t1 == t2 || root(t1) == root(t2)

  def root(t: Int): Int = nodes(t).parent match {
    case None => t
    case Some(p) => {
      nodes(t).parent = Some(root(p))
      nodes(t).parent.get
    }
  }
}

class Sushi() {

  private case class Order(val order: Array[String])

  private var orders = Array.empty[(Long, Order)];

  def come(n: Long, a: Array[String]): Unit = {
    orders = (orders.takeWhile(_ match { case (x: Long, _) => x < n }) :+ (n, Order(a))) ++ orders.dropWhile(_ match { case (x: Long, _) => x <= n })
  }

  def takeout(s: String): Long = {
    orders.filter(_ match { case (_, order) => order.order.contains(s) }).headOption match {
      case Some((id, order)) => {
        come(id, order.order.takeWhile(_ != s) ++ order.order.dropWhile(_ != s).tail)
        id
      }
      case None => -1
    }
  }

  def leave(id: Long) = {
    orders = orders.filter(_._1 != id)
  }
}

object Main {
  def solve(sc: => Scanner): Unit = {
    val A = Array.fill(3)(sc.nextLong).sorted
    println(A.max - A.min)
  }

  def recursive(N: Long, s: Stream[Long]): (Long, Long) = {
    if (N < s.head * s.head) ((1, N)) else if (N % s.head == 0) ((N / s.head, s.head)) else (recursive(N, s.tail))
  }

  def recursive2(N: Long): String = {
    if (N == 0) ("") else (('a'.toInt + (N % 26)).toChar + recursive2(N / 26))
  }

  def check(A: Array[List[Int]]): Boolean = {
    A.map(_.size == 0).fold(true)((a, b) => a & b)
  }

  def shift(n: Long): Long = {
    if (n == 0) (0) else if (n == 1) (1) else (shift(n - 1) << 1)
  }

  def unShift(n: Long): Long = {
    if (n == 0) (0) else (unShift(n >> 1) + 1)
  }

  def gcd(i: Long, j: Long): Long = {
    if (i < j) (gcd(j, i)) else (if (j == 0) (i) else (gcd(j, i % j)))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}

object Util {
  def getPermutation(begin: Long = 0): Stream[Long] =
    Stream.cons(begin, getPermutation(begin + 1))

  def getPrimeList(): Stream[Long] =
    getPrimeListRecursive(getPermutation(begin = 2))

  private def getPrimeListRecursive(A: Stream[Long]): Stream[Long] =
    Stream.cons(A.head, getPrimeListRecursive(A.tail.filter(_ % A.head != 0)))

  def fib(a: Long = 0, b: Long = 1, mod: Long = Long.MaxValue): Stream[Long] = a #:: fib(b % mod, (a + b) % mod, mod)
}

object ArabicRoman {

  type =?>[A, B] = PartialFunction[A, B]

  val codeTable = List(
    (1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), (100, "C"),
    (90, "XC"), (50, "L"), (40, "XL"), (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I"))

  val arabicToRoman: (Int) =?> String = {
    case src if (src >= 1 && src <= 3999) => {

      def convert(left: Int, cont: String = "", code: List[(Int, String)] = codeTable): String = {
        val (unitVal, unitChar) = code.head
        left - unitVal match {
          case n if (n == 0) => cont + unitChar
          case n if (n > 0) => convert(n, cont + unitChar, code)
          case _ => convert(left, cont, code.tail)
        }
      }

      convert(src)
    }
  }

  val romanToArabic: (String) =?> Int = {
    case src if (Option(src).exists { s => {
      s.nonEmpty && ("""[^MDCLXVI]""".r.findFirstMatchIn(s.toUpperCase) == None)
    }
    }) => {

      def convert(left: String, cont: Int = 0, code: List[(Int, String)] = codeTable): Int = {
        val (unitVal, unitChar) = code.head
        left.splitAt(unitChar.length) match {
          case ("", _) => cont
          case (`unitChar`, tail) => convert(tail, cont + unitVal, code)
          case _ => convert(left, cont, code.tail)
        }
      }

      convert(src.toUpperCase())
    }
  }
}
