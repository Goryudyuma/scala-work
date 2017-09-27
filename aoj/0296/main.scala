import java.util.Scanner

object Main {


  def solve(sc: => Scanner): Unit = {
    println(if (sc.nextInt
      + sc.nextInt * 5
      + sc.nextInt * 10
      + sc.nextInt * 50
      + sc.nextInt * 100
      + sc.nextInt * 500
      >= 1000
    ) (1) else (0))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
