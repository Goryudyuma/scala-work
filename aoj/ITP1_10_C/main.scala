import java.util.Scanner

object Main {

  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt
    if (N != 0) {
      val A = Array.fill[Double](N)(sc.nextInt)
      val ave = A.sum / N: Double
      println(math.sqrt(A.map(X => (X - ave) * (X - ave)).sum / N))
      solve(sc)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}

