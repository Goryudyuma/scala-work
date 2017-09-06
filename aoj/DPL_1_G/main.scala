import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val N, W = sc.nextInt
    val dp = Array.fill[Int](W + 1)(0)
    for (_ <- 0 until N) {
      val v, w, m = sc.nextInt
      var flag = true
      for (_ <- 0 until m) {
        if (flag) {
          flag = false
          for (i <- 0 to W - w) {
            val x = W - i
            val y = W - i - w
            if (dp(x) < dp(y) + v) {
              dp(x) = dp(y) + v
              flag = true
            }
          }
        }
      }
    }
    println(dp(W))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
