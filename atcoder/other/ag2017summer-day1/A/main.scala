import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt
    println(if (N <= 52) (N + N / 2) else {
      var N_ = N: Long;
      var minus = 26L
      var ans = 0L;
      while (N_ > 0L) {
        ans += N_;
        N_ -= minus;
        minus *= 26L
      }
      ans
    })
  }


  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
