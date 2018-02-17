import java.util.Scanner

import scala.collection.Searching._
import scala.annotation.tailrec
import scala.collection.immutable.Queue
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
  def solve(sc: => Scanner): Unit = {
    val X, Y = sc.nextInt
    var que = mutable.Queue[(Int, Int)]()
    que += ((10, 10))
    val P = Array.fill(20)(Array.fill(20)(-1))
    P(10)(10) = 0
    val Q = Array((2, 1), (1, 2), (2, -1), (-1, 2), (-2, 1), (1, -2), (-1, -2), (-2, -1))
    while (que.size > 0) {
      val now = que.dequeue
      if (P(now._1)(now._2) < 3) (
        for (diff <- Q) {
          val next = (now._1 + diff._1, now._2 + diff._2)
          if (P(next._1)(next._2) == -1) {
            P(next._1)(next._2) = P(now._1)(now._2) + 1
            que += next
          }
        })
    }
    println(if (0 <= X + 10 && X + 10 < 20 && 0 <= Y + 10 && Y + 10 < 20 && P(X + 10)(Y + 10) != -1) ("YES") else ("NO"))
  }

  def gcd(i: Int, j: Int): Int = {
    if (i < j) (gcd(j, i)) else (if (j == 0) (i) else (gcd(j, i % j)))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
