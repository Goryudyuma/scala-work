import java.util.Scanner

object Main {

  def solve(sc: => Scanner): Unit = {
    val D, L = sc.nextInt
    println(((D / L): Int) + D % L)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
