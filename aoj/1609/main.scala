import java.util.Scanner

/**
  * Created by goryudyuma on 2017/07/16.
  */

object Main {

  def solve(sc: Scanner): Unit = {
    val n: Int = sc.nextInt()
    if (n != 0) {
      sc.nextLine()
      val S: Array[String] = sc.nextLine().split(" ")
      var m: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map[String, Int]()

      for (s <- S) {
        m = m + (s -> 0)
      }

      var ans = "";
      var ansnum = -1
      for (i <- 0 until n) {

        m = m + (S(i) -> (m(S(i)) + 1))

        var num1 = 0
        var num2 = 0
        var str1 = ""
        var str2 = ""
        for (kv <- m) {
          var (k, v) = kv
          if (num1 < v) {
            val tmp = num1
            num1 = v
            v = tmp

            val tmp2 = str1
            str1 = k
            k = tmp2
          }

          if (num2 < v) {
            val tmp = num2
            num2 = v
            v = tmp

            val tmp2 = str2
            str2 = k
            k = tmp2
          }
        }

        if (ans.equals("") && num2 + n - i - 1 < num1) {
          ans = str1
          ansnum = i + 1
        }
      }

      if (ansnum == -1) {
        println("TIE")
      } else {
        println(ans + " " + ansnum)
      }
      solve(sc)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
