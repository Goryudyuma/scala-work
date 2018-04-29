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
  def solve(sc: => Scanner): Unit = {
    val A = Array.fill(sc.nextInt)(sc.nextInt)
    var nowR = 0
    var nowL = (A.size - 1) * 10
    var flag = true
    for (i <- Range(0, A.size)) {
      if (i * 10 <= nowR) {
        nowR = math.max(nowR, i * 10 + A(i))
      } else {
        flag = false
      }
      if ((A.size - i - 1) * 10 >= nowL) {
        nowL = math.min(nowL, (A.size - i - 1) * 10 - A(A.size - i - 1))
      } else {
        flag = false
      }
    }
    if (flag) {
      println("yes")
    } else {
      println("no")
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
