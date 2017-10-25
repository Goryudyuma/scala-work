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
  def solve(sc: => Scanner): Unit = {
    val A = Array.fill(sc.nextInt)(sc.nextInt)
    var ans = -1
    for (i <- 0 until A.size) {
      for (j <- i + 1 until A.size) {
        if (check(A(i) * A(j))) ans = math.max(ans, A(i) * A(j))
      }
    }
    println(ans)
  }

  def check(x: Int): Boolean = {
    
    var now = x
    var now1 = x % 10
    var f = true
    while (now >= 10) {
      val now2 = (now % 100 / 10): Int
      //println(now1, now2)
      if (now1 != now2 + 1) (f = false)
      now1 = now2
      now = (now / 10): Int
    }
    f

  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
