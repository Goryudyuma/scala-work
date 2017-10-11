import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt
    if (N != 0) {
      val y = sc.nextDouble
      var max = 0.0
      var ans = 0
      for (_ <- 0 until N) {
        val b = sc.nextInt
        val r, t = sc.nextDouble
        if (t == 1) (if (max < 1 + y * r / 100) {
          max = 1 + y * r / 100
          ans = b
        })
        else (if (max < math.pow((1 + r / 100), y)) {
          max = math.pow((1 + r / 100), y)
          ans = b
        })
      }
      println(ans)
      solve(sc)
    }
  }


  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
