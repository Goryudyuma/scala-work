import java.util.Scanner

object Main {

  def solve2(N: Int, M: Int): (Int, Int) = {
    if (N < M)
      solve2(M, N)
    else if (M == 0)
      (N, 0)
    else {
      val ret = solve2(M, N % M)
      (ret._1, ret._2 + 1)
    }
  }

  def solve(sc: => Scanner): Unit = {
    val N, M = sc.nextInt
    if (N != 0 && M != 0) {
      val ans = solve2(N, M)
      println(ans._1 + " " + ans._2)
      solve(sc)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
