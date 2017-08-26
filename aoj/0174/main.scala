import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val S = sc.next
    if (S != "0") {
      val A = S.drop(1).filter(c => 'A' == c).length
      val B = S.drop(1).filter(c => 'B' == c).length
      if (A < B)
        println(A + " " + (B + 1))
      else
        println((A + 1) + " " + B)
      solve(sc)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
