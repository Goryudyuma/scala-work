import java.util.Scanner

object Main {

  def solve(sc: => Scanner): Unit = {
    val A = Array.fill[Int](9)(0)
    A(0) = sc.nextInt
    A(1) = sc.nextInt
    A(4) = sc.nextInt
    A(2) = A(4) * 3 - A(0) - A(1)
    A(8) = A(4) * 3 - A(0) - A(4)
    A(7) = A(4) * 3 - A(1) - A(4)
    A(5) = A(4) * 3 - A(2) - A(8)
    A(3) = A(4) * 3 - A(5) - A(4)
    A(6) = A(4) * 3 - A(0) - A(3)
    for (i <- 0 until 3) {
      println(A(i * 3) + " " + A(i * 3 + 1) + " " + A(i * 3 + 2))
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
