import java.util.Scanner

object Main {
  def solve2(sc: => Scanner): Array[String] = {
    if (!sc.hasNext) (Array[String]()) else {
      val S = sc.next
      var ret = solve2(sc)

      var SS = S.split('@')
      if (SS.size >= 2) {
        SS = SS.drop(1)
        while (SS.length != 0) {
          SS = SS.dropWhile(s => s.length == 0)
          SS.headOption match {
            case Some(a) => ret :+= a
          }
          SS = SS.drop(1)
        }
      }

      ret
    }
  }

  def solve(sc: => Scanner): Unit = {
    println(solve2(sc).sorted.distinct.mkString("\n"))
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
