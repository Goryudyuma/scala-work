import java.util.Scanner

object Main {

  def check2(num: Int, input: Array[Array[Int]]): Int = {
    var ret = 0
    for (j <- 0 until input(0).length) {
      var N = 0
      for (i <- 0 until input.length) {
        if ((num >> i) % 2 == 1) {
          N += 1 - input(i)(j)
        } else {
          N += input(i)(j)
        }
      }
      ret += math.max(N, input.length - N)
    }
    ret
  }

  def make(input: Array[Array[Int]]): Int = {
    var max: Int = 0
    for (i <- 0 to (1 << (input.length - 2))) {
      max = math.max(max, check2(i, input))
    }
    max
  }

  def solve(sc: Scanner): Boolean = {
    val R: Int = sc.nextInt
    val C: Int = sc.nextInt
    if (R != 0 && C != 0) {
      println(make(Array.fill(R)(Array.fill(C)(sc.nextInt))))

      true
    } else {
      false
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    while (solve(sc)) ()
  }
}
