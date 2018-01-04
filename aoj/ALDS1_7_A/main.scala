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
    val N = sc.nextInt
    val parent = Array.fill(N)(-1)
    val children = Array.fill(N)(Array.fill(0)(0))
    for (_ <- Range(0, N)) {
      children(sc.nextInt) = Array.fill(sc.nextInt)(sc.nextInt)
    }
    for (i <- Range(0, N)) {
      for (x <- children(i)) {
        parent(x) = i
      }
    }
    for (i <- Range(0, N)) {
      var now = i
      var depth = 0
      while (parent(now) != -1) {
        now = parent(now)
        depth += 1
      }
      var typeName = "internal node"
      if (children(i).size == 0) (typeName = "leaf")
      if (depth == 0) (typeName = "root")
      println("node " + i + ": parent = " + parent(i) + ", depth = " + depth + ", " + typeName + ", [" + children(i).mkString(", ") + "]")
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}

