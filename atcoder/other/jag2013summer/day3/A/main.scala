import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val N, D = sc.nextInt
    var X = sc.nextInt
    var prev = Array.fill[Int](N)(sc.nextInt)
    for (idx <- 0 until D - 1) {
      val now = Array.fill[Int](N)(sc.nextInt)
      val DP = Array.tabulate[Int](X + 1)(x => x)
      for (i <- 0 until N) {
        for (j <- 0 to X - prev(i)) {
          DP(j + prev(i)) = math.max(DP(j + prev(i)), DP(j) + now(i))
        }
      }
      //println(DP.mkString(" "))
      for (i <- 0 until DP.size) {
        X = math.max(X, DP(i) + DP.size - 1 - i)
      }
      prev = now
    }
    println(X)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
