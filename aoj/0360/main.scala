import java.util.Scanner

import scala.collection.Searching._
import scala.annotation.tailrec
import scala.collection.immutable.Queue

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
  def solve2(sc: => Scanner, x: Int, y: Int, n: Int): Boolean = {
    if (n == 0) (true) else {
      val x_, y_ = sc.nextInt
      if ((x_ <= x && x < y_) || (x_ < y && y <= y_) || (x <= x_ && x_ < y) || (x < y_ && y_ <= y)) {
        false
      } else (solve2(sc, x, y, n - 1))
    }
  }

  def solve(sc: => Scanner) = {
    val x, y, n = sc.nextInt
    if (solve2(sc, x, y, n)) (println(0)) else (println(1))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}

