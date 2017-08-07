import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val S = sc.next
    println("" + S.head + (S.length - 2) + S.last)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
