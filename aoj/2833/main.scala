import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Long = {
    val N, K = sc.nextInt
    val A = Array.fill(N)(sc.nextInt - 1)
    val Ans = A.map(X => X == -1)
    val leaf = Array.fill(N)(true)
    for (i <- 0 until N) {
      if (A(i) != -1) (leaf(A(i)) = false)
    }
    val memo = Array.fill(N)(K * 2)
    var i = 0
    while (i < N) {
      if (leaf(i)) {
        var now = i
        var idx = 0
        while (now != -1 && idx < K) {
          Ans(now) = true
          now = A(now)
          idx += 1
          if (now != -1) {
            if (memo(now) <= idx) {
              now = -1
            } else {
              memo(now) = idx
            }
          }
        }
      }
      i += 1
    }

    i = 0
    var ans = 0
    while (i < N) {
      if (Ans(i)) {
        ans += 1
      }
      i += 1
    }
    ans
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    println(solve(sc))
  }
}
