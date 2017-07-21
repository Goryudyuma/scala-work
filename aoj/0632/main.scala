import java.util.Scanner

object Main {

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    val N: Int = sc.nextInt
    val M: Int = sc.nextInt
    val D: Int = sc.nextInt
    val A: Array[String] = Array.fill[String](N)(sc.next)

    var ans = 0
    for (i <- 0 until N) {
      for (j <- 0 to M - D) {
        var flag = true
        for (k <- 0 until D) {
          if (A(i)(j + k) != '.') {
            flag = false
          }
        }
        if (flag) {
          ans += 1
        }
      }
    }
    for (i <- 0 to N - D) {
      for (j <- 0 until M) {
        var flag = true
        for (k <- 0 until D) {
          if (A(i + k)(j) != '.') {
            flag = false
          }
        }
        if (flag) {
          ans += 1
        }
      }
    }
    println(ans)
  }
}
