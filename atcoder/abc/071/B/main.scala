import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val S = sc.next
    println("abcdefghijklmnopqrstuvwxyz".filter(c => !S.exists(c == _)).headOption.getOrElse("None"))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
