import java.util.Scanner

object Main {

  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt
    var M = sc.nextInt
    var ans = M
    for (_ <- 0 until N) {
      M += sc.nextInt - sc.nextInt
      if (M < 0) (ans = -1) else (if (ans != -1) (ans = math.max(M, ans)))
    }
    println(math.max(ans, 0))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
