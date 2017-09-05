import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    println(sc.next.zipWithIndex.filter(a => a._2 % 2 == 0).map(a => a._1).mkString)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
