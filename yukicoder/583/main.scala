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
    case Some(p) => root(p)
  }
}

object Main {
  val m = (1 << 9) - 1

  def solve(sc: => Scanner): Unit = {
    val N, M = sc.nextInt
    var uf = new IUnionFind(N)
    val A = Array.fill(N)(0)
    for (_ <- 0 until M) {
      val x, y = sc.nextInt
      A(x) += 1
      A(y) += 1
      uf = uf.union(x, y)
    }

    var X = Array[Int]()
    for (i <- 0 until N) {
      if (uf.root(i) != i) {
        X :+= uf.root(i)
      }
    }

    val s = A.filter(x => x % 2 == 1).size
    println(if (X.toSet.size == 1 && (s == 0 || s == 2)) ("YES") else ("NO"))
  }

  def recursive(N: Long, A: Array[Int], now: Long, sc: => Scanner): Long = {
    if (N == 0) (0) else (
      A.head * now + recursive(N - 1, A.tail, now - sc.nextInt + sc.nextInt, sc)
      )
  }

  def recursive2(X: Long, prime: Long): (Long, Long) = {
    if (X % prime != 0) ((1, X)) else {
      if (X % (prime * prime) == 0) (recursive2(X / prime / prime, prime)) else ((prime, X / prime))
    }
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

  def calc(v: Long): Long = (v + 1) * v / 2

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
