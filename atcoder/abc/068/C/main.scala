import java.util.Scanner

object Main {
  def calc(x: Int): Int = if (x == 1) (1) else (calc(x / 2) * 2)

  def solve(sc: => Scanner): Unit = {
    val N, M = sc.nextInt
    var A = Set(0)
    var B = Set(N - 1)
    for (i <- 0 until M) {
      val x, y = sc.nextInt - 1
      if (x == 0) {
        A += y
      } else if (x == N - 1) {
        B += y
      } else if (y == 0) {
        A += x
      } else if (y == N - 1) {
        B += x
      }
    }
    if ((A & B).size != 0) (println("POSSIBLE")) else (println("IMPOSSIBLE"))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
