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
    val S = sc.next
    var count = 0
    for (_ <- Range(0, N)) {
      val s = sc.next
      var flag = true
      for (i <- Range(0, math.max(0, s.length - S.length) + 1)) {
        if (flag) {
          for (j <- Range(1, 200)) {
            if (flag) {
              if (i + j * (S.length - 1) < s.length) {
                //println(i, j)
                var f = true

                for (k <- Range(0, S.length)) {
                  if (s(i + j * k) != S(k)) (f = false)
                }

                if (f) {
                  count += 1
                  flag = false
                }
              }
            }
          }
        }
      }
    }
    println(count)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}

