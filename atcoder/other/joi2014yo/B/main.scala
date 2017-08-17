import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val N, M = sc.nextInt
    val K = Array.fill[Int](N)(sc.nextInt)
    val C = Array.fill[Int](N)(0)
    for (idx <- 0 until M) {
      var X = sc.nextInt
      for (i <- 0 until N) {
        if (K(i) <= X) {
          X = -1
          C(i) += 1
        }
      }
    }
    var ans = 0
    for (i <- 0 until N) {
      if (C(ans) < C(i)) {
        ans = i
      }
    }
    println(ans + 1)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
