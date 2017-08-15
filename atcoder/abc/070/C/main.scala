import java.util.Scanner

object Main {
  def gcd(a: Long, b: Long): Long = if (a < b) (gcd(b, a)) else if (b == 0) (a) else (gcd(b, a % b))

  def solve(sc: => Scanner, N: Int): Long = {
    if (N == 0) (1) else {
      val X = sc.nextLong
      val Y = solve(sc, N - 1)
      X / gcd(X, Y) * Y
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    println(solve(sc, sc.nextInt))
  }
}
