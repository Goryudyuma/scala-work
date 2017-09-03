import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    println(math.max(sc.nextInt - sc.nextInt, 0))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
