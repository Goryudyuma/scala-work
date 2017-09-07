import java.util.Scanner

object Main {

  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt
    val A = Array.fill[Int](100003)(0)
    for (_ <- 0 until N) {
      val x = sc.nextInt
      A(x) += 1
      A(x + 1) += 1
      A(x + 2) += 1
    }
    println(A.max)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
