import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    println((math.pow(sc.nextInt, 2) - sc.nextInt).toInt)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
