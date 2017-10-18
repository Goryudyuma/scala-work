import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Long = {
    val N, D = sc.nextInt
    val A = Array.fill[(Long, Long)](N)((sc.nextLong, sc.nextLong))
    var now: Long = 1
    var people: Long = 0
    var ans: Long = 0
    var prevTime: Long = 0
    for (i <- 0 until N) {
      val time = math.abs(now - A(i)._2)
      if (time > A(i)._1 - prevTime) {
        return -1
      } else if (now - 1 + A(i)._2 - 1 <= A(i)._1 - prevTime) {
        ans += (now - 1) * people
        people = 0
      } else {
        ans += (A(i)._1 - prevTime) * people
      }
      people += 1
      if (people > D) {
        return -1
      }
      now = A(i)._2
      prevTime = A(i)._1
    }
    ans + people * (now - 1)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    println(solve(sc))
  }
}
