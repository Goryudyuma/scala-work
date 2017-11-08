import java.util.Scanner
import scala.collection.Searching._

import scala.annotation.tailrec

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
  def solve(sc: => Scanner, M: Array[Int]): Unit = {
    val N = sc.nextInt
    if (N != 0) {
      var i = 0
      var j = 0
      var count = 0
      while (i < M.size && j < M.size) {
        if (M(j) - M(i) == N) {
          count += 1
          i += 1
          j += 1
        } else if (M(j) - M(i) < N) {
          j += 1
        } else {
          i += 1
        }
      }
      println(count)
      solve(sc, M)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc, initialize())
  }

  def initialize(): Array[Int] = {
    val p = primes()
    for (i <- 1 until p.length - 1) {
      p(i) += p(i - 1)
    }
    p
  }

  def primes(): Array[Int] = {
    val ret = Array.fill(10001)(true)
    ret(1) = false
    for (i <- 2 until 10001) {
      if (ret(i)) {
        for (j <- 2 to 10001 / i) {
          if (i * j < 10001) {
            ret(i * j) = false
          }
        }
      }
    }
    ret.zipWithIndex.filter(A => A._1).map(A => A._2)
  }
}
