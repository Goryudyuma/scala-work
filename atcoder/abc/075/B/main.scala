import java.util.Scanner

object Main {
  def solve(sc: => Scanner): Unit = {
    val H, W = sc.nextInt
    val A = Array.fill[Array[Char]](H)(sc.next.toArray)
    for (i <- 0 until H) {
      for (j <- 0 until W) {
        print(if (A(i)(j) == '.') {
          var count = 0
          for (k <- -1 to 1) {
            for (l <- -1 to 1) {
              if (0 <= i + k && i + k < H && 0 <= j + l && j + l < W && A(i + k)(j + l) == '#') {
                count += 1
              }
            }
          }
          count
        } else ('#')
        )
      }
      println
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc)
  }
}
