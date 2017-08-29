import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val x, a, b = sc.nextInt
    println(if (Math.abs(x - a) < Math.abs(x - b)) "A" else "B")
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
