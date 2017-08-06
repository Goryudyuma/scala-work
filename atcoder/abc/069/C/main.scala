import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt
    val A = Array.fill[Int](N)(sc.nextInt)
    val X0 = A.filter(x => x % 2 != 0).length
    val X4 = A.filter(x => x % 4 == 0).length
    val X2 = N - X0 - X4
    if ((if (X2 != 0) (0) else (-1)) + X0 <= X4) (println("Yes")) else (println("No"))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
