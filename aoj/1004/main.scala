import java.util.Scanner

object Main {
  def isPrime(p: Int) = if (p == 1) {
    false
  } else {
    (2 to math.sqrt(p).toInt).forall(p % _ != 0)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    while (sc.hasNext()) {
      val n: Int = sc.nextInt
      var ans = 0
      for (i <- 1 to n) {
        if (isPrime(i) && isPrime(n - i + 1)) {
          ans += 1
        }
      }
      println(ans)
    }
  }
}
