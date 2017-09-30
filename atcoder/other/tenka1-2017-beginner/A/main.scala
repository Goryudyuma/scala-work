import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    println(sc.next.filter(_ == '1').size)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
