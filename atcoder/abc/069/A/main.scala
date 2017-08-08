import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    println((sc.nextInt - 1) * (sc.nextInt - 1))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
