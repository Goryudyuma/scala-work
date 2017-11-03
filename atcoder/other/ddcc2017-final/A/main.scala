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
  def check(x: Int, y: Int, n: Int): Boolean = {
    n * n >= x * x + y * y
  }


  def solve2(K: Int, N: Int): Int = {
    var count = 0
    for (i <- 0 until N / K) {
      for (j <- 0 until N / K) {
        val X = (i * K - N / 2);
        val Y = (j * K - N / 2);
        if (check(X, Y, N / 2) && check(X + K, Y, N / 2) && check(X, Y + K, N / 2) && check(X + K, Y + K, N / 2)) {
          count += 1
        }
      }
    }
    count
  }

  def solve(sc: => Scanner): Unit = {
    val K = sc.nextInt
    println(solve2(K, 200) + " " + solve2(K, 300))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
