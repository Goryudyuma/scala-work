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
  def solve(sc: => Scanner) = {
    val N = sc.nextInt
    var H = sc.nextLong
    val A, B = Array.fill(N)(0L)
    for (i <- Range(0, N)) {
      A(i) = sc.nextLong
      B(i) = sc.nextLong
    }
    val maxA = A.sorted.reverse(0)
    var ans = 0L
    for (x <- B.filter(x => x > maxA).sorted.reverse) {
      if (H > 0) {
        ans += 1
        H -= x
      }
    }
    if (H > 0) {
      ans += (H - 1) / maxA + 1
    }
    println(ans)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}

