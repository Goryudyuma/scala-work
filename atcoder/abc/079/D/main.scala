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
  def solve(sc: => Scanner): Unit = {
    val H, W = sc.nextInt
    val A = Array.fill(10)(Array.fill(10)(sc.nextInt))
    for (k <- 0 until 10) {
      for (i <- 0 until 10) {
        for (j <- 0 until 10) {
          A(i)(j) = math.min(A(i)(j), A(i)(k) + A(k)(j))
        }
      }
    }
    for (k <- 0 until 10) {
      for (i <- 0 until 10) {
        for (j <- 0 until 10) {
          A(i)(j) = math.min(A(i)(j), A(i)(k) + A(k)(j))
        }
      }
    }
    val B = Array.fill(H * W)(sc.nextInt).filter(a => a != -1)
    var ans = 0
    for (i <- 0 until 10) {
      ans += B.count(a => a == i) * A(i)(1)
    }
    println(ans)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
