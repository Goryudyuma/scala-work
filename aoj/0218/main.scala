import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt
    if (N != 0) {
      for (idx <- 0 until N)
        println((sc.nextInt, sc.nextInt, sc.nextInt) match {
          case (m, e, j) if (m == 100 || e == 100 || j == 100 || m + e >= 180 || m + e + j >= 240) => "A"
          case (m, e, j) if (m + e + j >= 210 || (m + e + j >= 150 && (m >= 80 || e >= 80))) => "B"
          case _ => "C"
        })
      solve(sc)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
