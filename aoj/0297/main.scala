import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = Array.fill(sc.nextInt)((sc.nextInt, sc.nextInt)).map(a => (a._1 - 1) % a._2 + 1).map(println(_))
  
  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
