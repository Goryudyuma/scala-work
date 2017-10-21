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
  def binarySearchIterative(list: Array[Double], target: Double): Int = {
    var left = 0
    var right = list.length - 1
    while (left <= right) {
      val mid = left + (right - left) / 2
      if (list(mid) > target)
        right = mid - 1
      else
        left = mid + 1
    }
    left
  }

  def binarySearchIterative2(list: Array[Double], target: Double): Int = {
    var left = 0
    var right = list.length - 1
    while (left <= right) {
      val mid = left + (right - left) / 2
      if (list(mid) >= target)
        right = mid - 1
      else
        left = mid + 1
    }
    left
  }

  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt * 2
    println(((N / 60): Int) + " " + N % 60)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
