import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt
    val len = Array.fill[Array[Int]](N)(Array.fill[Int](N)(Int.MaxValue / 6))
    for (idx <- 0 until N) {
      len(idx)(idx) = 0
      val now = sc.nextInt - 1
      val count = sc.nextInt
      for (idx2 <- 0 until count) {
        len(now)(sc.nextInt - 1) = 1
      }
    }

    for (k <- 0 until N) {
      for (i <- 0 until N) {
        for (j <- 0 until N) {
          len(i)(j) = Math.min(len(i)(j), len(i)(k) + len(k)(j))
        }
      }
    }

    val P = sc.nextInt
    for (idx <- 0 until P) {
      val s, d = sc.nextInt - 1
      val v = sc.nextInt
      if (len(s)(d) + 1 <= v) {
        println(len(s)(d) + 1)
      } else {
        println("NA")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
