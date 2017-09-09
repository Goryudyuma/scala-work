import java.util.Scanner

object Main {

  def solve(sc: => Scanner): Unit = {
    println(if ((sc.next * 2).contains(sc.next)) ("Yes") else ("No"))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
