import java.util.Scanner

object Main {

  def solve(sc: => Scanner): Unit = {
    val N, M = sc.nextInt
    if (N != 0 && M != 0) {
      var ans = 0;
      val A = Array.fill[Array[Int]](N)(Array.fill[Int](M)(sc.nextInt));
      for (i <- 0 until N - 2) {
        for (j <- 0 until M - 2) {
          for (k <- i + 2 until N) {
            for (l <- j + 2 until M) {
              var minv = 1 << 30;
              for (m <- i to k) {
                minv = math.min(minv, A(m)(j));
                minv = math.min(minv, A(m)(l));
              }

              for (m <- j to l) {
                minv = math.min(minv, A(i)(m));
                minv = math.min(minv, A(k)(m));
              }


              var sum = 0;
              for (m <- i + 1 until k) {
                for (n <- j + 1 until l) {
                  if (A(m)(n) >= minv) {
                    sum = -1000000000;
                  }
                  sum += minv - A(m)(n);
                }
              }
              ans = math.max(ans, sum);
            }
          }
        }
      }
      println(ans);
      solve(sc);
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
