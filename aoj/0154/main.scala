import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val M = sc.nextInt
    if (M != 0) {
      val X = Array.fill[Int](10000)(0)
      X(0) = 1
      for (_ <- 0 until M) {
        val A, B = sc.nextInt
        for (j <- 0 to 10000 - A - 1) (for (i <- 1 to B) (if (10000 - A * i - 1 - j >= 0) (X(10000 - 1 - j) += X(10000 - A * i - 1 - j))))
      }
      val N = sc.nextInt
      for (_ <- 0 until N) {
        println(X(sc.nextInt))
      }
      solve(sc)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
