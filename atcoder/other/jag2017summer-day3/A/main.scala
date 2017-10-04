import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val S = sc.next.dropWhile(_ != '*').drop(1).groupBy(a => a)
    println((S.getOrElse(')', "")).length - (S.getOrElse('(', "")).length)
  }


  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
