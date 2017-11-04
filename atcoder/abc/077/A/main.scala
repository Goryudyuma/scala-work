拡げる

Copy
import java.util.Scanner
 
object Main {
  def solve(sc: => Scanner): Unit =
    println(if (sc.next.reverse == sc.next) ("YES") else ("NO"))
 
  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
 
    solve(sc)
  }
}
