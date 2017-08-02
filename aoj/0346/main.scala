import java.util.Scanner

object Main {
  def calc(x: Int): Int = if (x == 1) (1) else (calc(x / 2) * 2)

  def solve(sc: => Scanner): Unit = {
    val A = Array.fill[Int](12)(sc.nextInt).sorted
    if (A(0) == A(1) && A(1) == A(2) && A(2) == A(3)
      && A(4) == A(5) && A(5) == A(6) && A(6) == A(7)
      && A(8) == A(9) && A(9) == A(10) && A(10) == A(11))
      (println("yes")) else (println("no"))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
