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
    val H, W = sc.nextInt
    val A = Array.fill(H)(sc.next)
    val B = Array((1, 0), (0, 1), (-1, 0), (0, -1))
    println(if ((0 until H).map(i =>
      (0 until W).map(j =>
        if (A(i)(j) == '#') {
          (0 until 4).map { k =>
            val now = (B(k)._1 + i, B(k)._2 + j)
            if (0 <= now._1 && now._1 < H && 0 <= now._2 && now._2 < W && A(now._1)(now._2) == '#') (true) else (false)
          }.fold(false)((a, b) => a | b)
        } else (true)
      ).fold(true)((a, b) => a & b)
    ).fold(true)((a, b) => a & b)) ("Yes") else ("No"))
  }

  def recursive(now: Int, A: Array[List[Int]]): Int = {
    if (A(now).size == 0) (now) else {
      val next = A(now).head
      A(now) = A(now).filter(_ != next)
      A(next) = A(next).filter(_ != now)
      recursive(next, A)
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
