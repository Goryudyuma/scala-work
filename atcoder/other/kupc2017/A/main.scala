import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt
    var K = sc.nextInt
    val ans = N - Array.fill[Int](N)(sc.nextInt).sorted.reverse.dropWhile(a => {
      K = K - a;
      K > 0
    }).size + 1
    println(if (ans > N) (-1) else (ans))
  }


  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
