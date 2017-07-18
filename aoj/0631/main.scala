import java.util.Scanner
object Main {
  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    val N = sc.nextInt()
    val M = sc.nextInt()
    var sum = 0
    var minV = 1 << 30
    for (i <- 0 until M) {
      val x = sc.nextInt()
      minV = math.min(minV, x)
      sum += math.max(N - x, 0)
      sc.nextInt()
    }
    sum -= N - minV
    println(sum)
  }
}
