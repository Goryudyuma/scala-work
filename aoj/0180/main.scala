import java.util.Scanner

import scala.collection.mutable

object Main {
  def solve(sc: => Scanner): Unit = {
    val N, M = sc.nextInt
    if (N != 0 || M != 0) {
      val list = Array.fill[List[(Int, Int)]](N)(List[(Int, Int)]())
      for (i <- 0 until M) {
        val a, b, cost = sc.nextInt
        list(a) :+= (b, cost)
        list(b) :+= (a, cost)
      }
      val que = mutable.PriorityQueue.empty[(Int, Int)](implicitly[Ordering[(Int, Int)]].reverse)
      que.enqueue((0, 0))
      val flag = Array.fill[Boolean](N)(false)
      var ans = 0
      while (que.length != 0) {
        val now = que.dequeue()
        if (!flag(now._2)) {
          flag(now._2) = true
          ans += now._1
          for (next <- list(now._2)) {
            if (!flag(next._1)) {
              que.enqueue((next._2, next._1))
            }
          }
        }
      }
      println(ans)
      solve(sc)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}
System.in)
    solve(sc)
  }
}
