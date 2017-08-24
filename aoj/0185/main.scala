import java.util.Scanner

object Main {
  val primeList = Array.fill[Boolean](1000001)(true)

  def construct = {
    primeList(0) = false
    primeList(1) = false
    for (i <- 2 until 1000001) {
      if (primeList(i)) {
        var j = 2
        while (i * j < 1000001) {
          primeList(i * j) = false
          j += 1
        }
      }
    }
  }

  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt
    if (N != 0) {
      var count = 0
      for (i <- 2 to N / 2) {
        if (primeList(i) && primeList(N - i)) {
          count += 1
        }
      }
      println(count)
      solve(sc)
    }
  }

  def main(args: Array[String]): Unit = {
    construct

    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}

