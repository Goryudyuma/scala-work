import java.util.Scanner

object Main {
  def solve2(i: Int, sc: => Scanner): (Int, Int) = {
    if (i == 0) ((0, 0)) else {
      val ret = solve2(i - 1, sc)
      val A, B = sc.next
      if (A == B) {
        (ret._1 + 1, ret._2 + 1)
      } else if (A < B) {
        (ret._1, ret._2 + 3)
      } else {
        (ret._1 + 3, ret._2)
      }
    }
  }

  def solve(sc: => Scanner): Unit = {
    val ans = solve2(sc.nextInt, sc)
    println(ans._1 + " " + ans._2)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
