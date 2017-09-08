import java.util.Scanner

object Main {

  def solve(sc: => Scanner): Unit = {
    val A1 = math.min(sc.nextLong * 4, math.min(sc.nextLong * 2, sc.nextLong))
    val A2 = math.min(A1 * 2, sc.nextLong)
    val N = sc.nextLong
    println((N / 2: Long) * A2 + N % 2 * A1)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
