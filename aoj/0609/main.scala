import java.util.Scanner

object Main {
  def solve(sc: Scanner, n: Int, m: Int, loop: Int, point: Array[Int], target: Array[Int]): Array[Int] = {
    if (m == loop) {
      point
    } else {
      for (i <- 0 until n) {
        val v: Int = sc.nextInt - 1
        if (v == target(loop)) {
          point(i) += 1
        } else {
          point(target(loop)) += 1
        }
      }
      solve(sc, n, m, loop + 1, point, target)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    val n: Int = sc.nextInt
    val m: Int = sc.nextInt
    val point: Array[Int] = new Array[Int](n)
    val target: Array[Int] = new Array[Int](m).map(_ => sc.nextInt - 1)
    solve(sc, n, m, 0, point, target).foreach(println)
  }
}
