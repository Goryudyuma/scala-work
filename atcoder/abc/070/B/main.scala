import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val A, B, C, D = sc.nextInt
    println(Math.max(0, (Math.min(B, D) - Math.max(A, C))))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
