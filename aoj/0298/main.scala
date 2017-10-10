import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit =
    println((List.fill[Int](sc.nextInt)(sc.nextInt * 60 + sc.nextInt) ::: List.fill[Int](sc.nextInt)(sc.nextInt * 60 + sc.nextInt))
          .toSet.toList
      .sorted
      .map(A => ((A / 60): Int) + ":%02d".format(A % 60))
      .mkString(" "))


  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
