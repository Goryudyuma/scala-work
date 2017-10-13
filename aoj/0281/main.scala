import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt
    for (_ <- 0 until N) {
      val c, a, n = sc.nextInt
      val ans1 = math.min(c, math.min(a, n))
      val ans2 = math.min(((c - ans1) / 2): Int, a - ans1)
      val ans3 = ((c - ans1 - ans2 * 2) / 3): Int
      println(ans1 + ans2 + ans3)
    }
  }


  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
