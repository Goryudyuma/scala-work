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
  def replace1(input: String): String = {
    if (input == "") ("") else {
      (input(0) match {
        case '0' => "1"
        case '1' => "2"
        case '2' => "3"
        case '3' => "4"
        case '4' => "5"
        case '5' => "6"
        case '6' => "7"
        case '7' => "8"
        case '8' => "9"
        case '9' => "0"
        case x => x
      }) + replace1(input.substring(1))
    }
  }

  def replace2(input: String): String = {
    if (input == "") ("") else {
      (input(0) match {
        case '0' => "9"
        case '1' => "0"
        case '2' => "1"
        case '3' => "2"
        case '4' => "3"
        case '5' => "4"
        case '6' => "5"
        case '7' => "6"
        case '8' => "7"
        case '9' => "8"
        case x => x
      }) + replace2(input.substring(1))
    }
  }

  def solve2(order: String, message: String): String = {
    if (order == "") (message) else {
      solve2(order.substring(1),
        order(0) match {
          case 'J' => message.charAt(message.length - 1) + message.substring(0, message.length - 1)
          case 'C' => message.substring(1) + message.charAt(0)
          case 'E' => message.substring((message.length + 1) / 2) + (if (message.length % 2 == 0) ("") else (message(message.length / 2))) + message.substring(0, message.length / 2)
          case 'A' => message.reverse
          case 'P' => replace2(message)
          case 'M' => replace1(message)
        })
    }
  }

  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt
    for (_ <- 0 until N) {
      println(solve2(sc.next.reverse, sc.next))
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
