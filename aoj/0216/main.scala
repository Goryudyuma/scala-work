import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt
    if (N != -1) {
      println(-(1150 + math.max((N - 10) * 125, 0) + math.max((N - 20) * 15, 0) + math.max((N - 30) * 20, 0) - 4280))
      solve(sc)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
