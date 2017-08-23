import java.util.Scanner

object Main {
  def solve2(sc: => Scanner, N: Int): Int = {
    if (N == 0) (0) else (
      solve2(sc, N - 1) + ((sc.nextInt, sc.nextInt, sc.nextInt, sc.nextInt) match {
        case (x, y, h, w) if (x + y + h <= 60 && w <= 2) => 600
        case (x, y, h, w) if (x + y + h <= 80 && w <= 5) => 800
        case (x, y, h, w) if (x + y + h <= 100 && w <= 10) => 1000
        case (x, y, h, w) if (x + y + h <= 120 && w <= 15) => 1200
        case (x, y, h, w) if (x + y + h <= 140 && w <= 20) => 1400
        case (x, y, h, w) if (x + y + h <= 160 && w <= 25) => 1600
        case _ => 0
      }))
  }

  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt
    if (N != 0) {
      println(solve2(sc, N))
      solve(sc)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}

