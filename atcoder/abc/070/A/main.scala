import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val S = sc.next
    println(if (S.reverse.equals(S)) ("Yes") else ("No"))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
