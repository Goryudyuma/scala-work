import java.util.Scanner

object Main {

  def solve(sc: => Scanner): Unit = {
    val N, M = sc.nextInt
    if (N != 0 && M != 0) {
      var maxV: Int = -1
      val A = Array.fill[Int](N)(sc.nextInt)
      for (i <- 0 until A.length) {
        for (j <- i + 1 until A.length) {
          if (A(i) + A(j) <= M) (
            maxV = math.max(maxV, A(i) + A(j))
            )
        }
      }
      if (maxV == -1) (println("NONE")) else (println(maxV))

      solve(sc)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
