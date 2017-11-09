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
    for (_ <- 0 until N) {
      val x, y = sc.nextInt
      val A = Array.fill(x + 1)(Array.fill(y + 1)(0L))
      A(0)(0) = 1
      val matatabi = Array.fill(sc.nextInt)((sc.nextInt, sc.nextInt), (sc.nextInt, sc.nextInt))

      for (i <- 0 to x) {
        for (j <- 0 to y) {
          if (i - 1 >= 0 && matatabi.find(x => x == ((i, j), (i - 1, j))) == None && matatabi.find(x => x == ((i - 1, j), (i, j))) == None) {
            A(i)(j) += A(i - 1)(j)
          }
          if (j - 1 >= 0 && matatabi.find(x => x == ((i, j), (i, j - 1))) == None && matatabi.find(x => x == ((i, j - 1), (i, j))) == None) {
            A(i)(j) += A(i)(j - 1)
          }
        }
        //println(A(i).mkString(" "))
      }
      println(if (A(x)(y) == 0) ("Miserable Hokusai!") else (A(x)(y)))
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }

}
