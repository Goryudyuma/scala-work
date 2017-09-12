import java.util.Scanner

object Main {

  def solve(sc: => Scanner): Unit = {
    sc.nextInt

    val A = Array.fill[Int](sc.nextInt)(sc.nextInt).toSet
    val B = Array.fill[Int](sc.nextInt)(sc.nextInt).toSet
    val C = Array.fill[Int](sc.nextInt)(sc.nextInt).toSet
    println(((B & C) | (C &~ A)).size)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
