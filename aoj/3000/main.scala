import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val A, B = sc.nextDouble
    val N: Double = (math.max(A, B) - math.min(A, B)) / 2 + math.min(A, B)
    println( if (math.abs(A - N) < 90) (N) else {
      if (N + 180 >= 360) (N - 180) else (N + 180)
    })
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}

