import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    var N = sc.nextInt
    while (N != 0) {
      if (N % 2 == 0) (System.gc)
      var z = true
      var sum = 0
      for (_ <- 0 until N) {
        val x = sc.nextInt
        if (x > 1) (z = false)
        if (x > 0) (sum += 1)
      }
      println(if (z) ("NA") else (sum + 1))

      N = sc.nextInt
    }
  }


  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
