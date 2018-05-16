import java.util.Scanner

import scala.collection.Searching._
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable._
import scala.io.StdIn.readLine

object Main {
  val m = (1 << 9) - 1

  def solve(sc: => Scanner): Unit = {
    println(Array.fill(2)(sc.nextInt).sum)
  }


  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}

