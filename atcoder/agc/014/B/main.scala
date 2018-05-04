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
    val A = Array.fill(N)(0)
    Array.fill(M) {
      val a, b = sc.nextInt - 1
      A(a) += 1
      A(b) += 1
    }

    println(if (A.map(_ % 2 == 0).fold(true)((a, b) => a & b)) ("YES") else ("NO"))
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
