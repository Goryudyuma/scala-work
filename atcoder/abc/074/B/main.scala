import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val N, K = sc.nextInt
    println(Array.fill[Int](N)(sc.nextInt).map(x => math.min(x, K - x) * 2).sum)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
