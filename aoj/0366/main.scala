import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    println((sc.nextInt, sc.nextInt) match {
      case (h, r) if -r > h => -1
      case (h, r) if h == -r => 0
      case _ => 1
    })
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
