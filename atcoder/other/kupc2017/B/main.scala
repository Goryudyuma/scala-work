import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val _, S = sc.nextInt
    var T = sc.nextInt
    println(if (S <= T) {
      var ans = 0
      while (S < T) {
        T /= 2
        ans = (ans + 1): Int
      }
      if (S == T) (
        ans) else (-1)
    } else (-1))
  }


  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
