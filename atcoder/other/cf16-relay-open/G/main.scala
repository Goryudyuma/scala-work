import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val N, Q = sc.nextInt
    val X = Array.tabulate[Int](N)(i => i)
    var Y = Set[Int](0, 1)
    var position1 = 0
    for (idx <- 0 until Q) {
      val A, B = sc.nextInt - 1
      if (A == position1) {
        position1 = B
      } else if (B == position1) {
        position1 = A
      }

      val Z = X(A)
      X(A) = X(B)
      X(B) = Z

      if (position1 != 0) Y += X(position1 - 1)
      if (position1 != N - 1) Y += X(position1 + 1)
    }
    println(Y.size)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
