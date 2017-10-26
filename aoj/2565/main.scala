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
    val A = Array.fill[String](sc.nextInt)(sc.next)
    if (A.length != 0) {
      var min = -10000000000000L
      var max = 10000000000000L
      var f = true
      for (i <- 0 until A.length - 1) {
        if (A(i) == "x" && A(i + 1) == "x") {
          f = false
        } else {
          if (i % 2 == 0) {
            if (A(i) != "x" && A(i + 1) != "x" && A(i).toInt > A(i + 1).toInt) {
              f = false
            } else if (A(i) == "x") {
              max = math.min(max, A(i + 1).toInt)
            } else if (A(i + 1) == "x") {
              min = math.max(min, A(i).toInt)
            }
          } else {
            if (A(i) != "x" && A(i + 1) != "x" && A(i).toInt < A(i + 1).toInt) {
              f = false
            } else if (A(i) == "x") {
              min = math.max(min, A(i + 1).toInt)
            } else if (A(i + 1) == "x") {
              max = math.min(max, A(i).toInt)
            }
          }
        }
      }
      if (f) {
        if (max - min == 2) {
          println(min + 1)
        } else {
          if (max - min < 2) {
            println("none")
          } else {
            println("ambiguous")
          }
        }
      } else {
        println("none")
      }

      solve(sc)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
