import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val D = Array.fill[(Int, Int)](sc.nextInt)((sc.nextInt, sc.nextInt)).max
    println(D._1 + D._2)
  }


  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
