import java.util.Scanner

import scala.collection.Searching._
import scala.annotation.tailrec
import scala.collection.immutable.{NumericRange, Queue}
import scala.collection.mutable
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

  @tailrec
  private def root(t: Int): Int = nodes(t).parent match {
    case None => t
    case Some(p) => root(p)
  }
}


object Main {
  def recursive(sc: => Scanner, D: => Int, day: Int, t: Long, k: Long): Long = {
    if (day == 0) (math.max(t, k)) else {
      val T, K = sc.nextLong
      recursive(sc, D, day - 1, math.max(t, k - D) + T, math.max(k, t - D) + K)
    }
  }

  def solve(sc: => Scanner): Unit = {
    val N, D = sc.nextInt
    println(recursive(sc, D, N, 0, Long.MinValue / 2))
  }

  def gcd(i: Long, j: Long): Long = {
    if (i < j) (gcd(j, i)) else (if (j == 0) (i) else (gcd(j, i % j)))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
