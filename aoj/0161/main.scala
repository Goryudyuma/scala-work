import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt
    if (N != 0) {
      val A = Array.fill[(Int, Int)](N)((sc.nextInt,
        sc.nextInt * 60 + sc.nextInt
          + sc.nextInt * 60 + sc.nextInt
          + sc.nextInt * 60 + sc.nextInt
          + sc.nextInt * 60 + sc.nextInt
      )).sortBy(_._2)
      println(A(0)._1)
      println(A(1)._1)
      println(A(A.size - 2)._1)
      solve(sc)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
