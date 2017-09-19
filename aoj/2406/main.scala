import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val N, T, E = sc.nextInt
    val ans = List.fill[Int](N)(sc.nextInt).indexWhere(x => ((T + E) / x) != ((T - E) / x))
    println(if (ans != -1) (ans + 1) else (ans))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
