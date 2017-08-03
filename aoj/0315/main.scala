import java.util.Scanner

object Main {
  def calc(x: Int): Int = if (x == 1) (1) else (calc(x / 2) * 2)

  def solve(sc: => Scanner): Unit = {
    println(Array.fill[Int](3)(sc.nextInt).sum)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
