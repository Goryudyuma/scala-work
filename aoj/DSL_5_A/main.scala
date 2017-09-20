import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val N, T = sc.nextInt
    val A = Array.fill[Int](T + 2)(0)
    for (_ <- 0 until N) {
      val r, l = sc.nextInt
      A(r) += 1
      A(l) -= 1
    }
    for (i <- 0 to T) {
      A(i + 1) += A(i)
    }
    println(A.max)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
