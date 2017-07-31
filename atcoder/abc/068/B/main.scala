import java.util.Scanner

object Main {
  def calc(x: Int): Int = if (x == 1) (1) else (calc(x / 2) * 2)

  def solve(sc: => Scanner): Unit = {
    println(calc(sc.nextInt))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
