import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val h1, h2 = sc.nextInt
    val k1, k2 = sc.nextInt
    val a, b, c, d = sc.nextInt
    val hiroshi = h1 * a + h1 / 10 * c + h2 * b + h2 / 20 * d
    val kenjiro = k1 * a + k1 / 10 * c + k2 * b + k2 / 20 * d
    if (hiroshi == kenjiro)
      (println("even"))
    else if (hiroshi > kenjiro)
      (println("hiroshi"))
    else
      (println("kenjiro"))

  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
