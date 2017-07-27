import java.util.Scanner

object Main {

  def gcd(a: Int, b: Int): Int = {
    if (b == 0) (a)
    else if (b > a) (gcd(b, a))
    else (gcd(b, a % b))
  }

  def solve(sc: => Scanner): Unit = {
    val A, B, C, D = sc.nextInt
    val X = math.abs(A - C)
    val Y = math.abs(B - D)
    println(X + Y - gcd(X, Y));
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
