import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val A, B, C = sc.nextInt
    println(if (A == B) (C) else (if (A == C) (B) else (A)))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
