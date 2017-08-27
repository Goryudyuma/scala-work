import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt
    println(Array.fill[Long](N)(sc.nextInt).sum / N: Long)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
