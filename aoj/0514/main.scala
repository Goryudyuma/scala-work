import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val a, b, c = sc.nextInt
    if (a != 0 && b != 0 && c != 0) {
      var X = Array.fill[Int](a + b + c)(2)
      val A = Array.fill[Array[Int]](sc.nextInt)(Array.fill[Int](4)(sc.nextInt - 1))
      for (now <- A) {
        if (now(3) == 0) {
          X(now(0)) = 1
          X(now(1)) = 1
          X(now(2)) = 1
        }
      }
      for (now <- A) {
        if (now(3) == -1) {
          if (X(now(0)) == 1 && X(now(1)) == 1) X(now(2)) = 0
          if (X(now(0)) == 1 && X(now(2)) == 1) X(now(1)) = 0
          if (X(now(2)) == 1 && X(now(1)) == 1) X(now(0)) = 0
        }
      }
      println(X.mkString("\n"))
      solve(sc)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
