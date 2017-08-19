import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val S = sc.next
    val N = sc.nextInt
    println(Array.fill[String](N)(sc.next).map(s => (s * 2).contains(S)).filter(b => b == true).length)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
