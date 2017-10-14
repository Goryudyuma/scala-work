import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = println((sc.next.groupBy(identity).mapValues(_.size).toSeq.filter(A => A._2 % 2 == 1).length / 2): Int)

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
