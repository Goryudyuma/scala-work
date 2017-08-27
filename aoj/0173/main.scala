import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    while (sc.hasNext) {
      val name = sc.next
      val a, b = sc.nextInt
      println(name + " " + (a + b) + " " + (a * 200 + b * 300))
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
