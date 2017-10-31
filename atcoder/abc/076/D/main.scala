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
    val N = sc.nextInt
    val speed = Array.fill(20002 * 2)(0.0)
    val maxspeed = Array.fill(20002 * 2)(0.0)
    val t = Array.fill(N)(sc.nextInt)
    val v = Array.fill(N)(sc.nextInt)

    var now = 0
    for (i <- 0 until N) {
      if (i != 0) {
        maxspeed(now) = math.min(v(i), v(i - 1))
      }
      for (j <- 1 until t(i) * 2) {
        maxspeed(now + j) = v(i)
      }
      now += t(i) * 2
    }
    var f = true
    while (f) {
      f = false
      for (i <- 1 to 20000 * 2) {
        if ((speed(i - 1) == speed(i) || speed(i - 1) - 0.5 == speed(i) || speed(i - 1) - 1 == speed(i)) && (speed(i + 1) == speed(i) || speed(i + 1) - 0.5 == speed(i) || speed(i + 1) - 1 == speed(i))) {
          if (speed(i) + 0.5 <= maxspeed(i)) {
            speed(i) += 0.5
            f = true
          }
        }
      }
    }
    println(speed.sum / 2)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
