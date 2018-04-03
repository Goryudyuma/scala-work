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

  def root(t: Int): Int = nodes(t).parent match {
    case None => t
    case Some(p) => root(p)
  }
}


object Main {
  def solve(sc: => Scanner): Unit = {
    val N = sc.nextLong
    if (N != -1) {
      solve2(sc, N = N, now = 0)
      solve(sc)
    }
  }

  val Data = Array[Long](
    Integer.parseInt("0111111", 2),
    Integer.parseInt("0000110", 2),
    Integer.parseInt("1011011", 2),
    Integer.parseInt("1001111", 2),
    Integer.parseInt("1100110", 2),
    Integer.parseInt("1101101", 2),
    Integer.parseInt("1111101", 2),
    Integer.parseInt("0100111", 2),
    Integer.parseInt("1111111", 2),
    Integer.parseInt("1101111", 2)
  )

  def solve2(sc: => Scanner, N: Long, now: Long): Unit = {
    if (N != 0) {
      val next = Data(sc.nextInt)
      println((now ^ next).toBinaryString.reverse.padTo(7, '0').reverse)
      solve2(sc, N - 1, next)
    }
  }

  def gcd(i: Long, j: Long): Long = {
    if (i < j) (gcd(j, i)) else (if (j == 0) (i) else (gcd(j, i % j)))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
