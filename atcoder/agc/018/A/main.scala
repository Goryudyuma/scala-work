import java.util.Scanner

object Main {

  def gcd(a: Int, b: Int): Int = {
    if (b == 0) (a)
    else if (a < b) (gcd(b, a))
    else (gcd(b, a % b))
  }

  def solve(sc: => Scanner): Unit = {
    val N, K = sc.nextInt()
    val D = Array.fill[Int](N)(sc.nextInt)
    if (D.max >= K && K % (D.foldLeft(0)((a, b) => (if (a == 0) (b) else (gcd(a, b))))) == 0) (
      println("POSSIBLE")
      ) else (
      println("IMPOSSIBLE")
      )
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
