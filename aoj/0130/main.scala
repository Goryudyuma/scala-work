import java.util.Scanner

import scala.collection.mutable

object Main {
  def solve(sc: => Scanner, N: Int): Unit = {
    if (N != 0) {
      val stack1 = mutable.Stack[Char]()
      val stack2 = mutable.Stack[Char]()
      var f: Boolean = false
      sc.next.foreach(c =>
        c match {
          case '<' => {
            if (!f) stack2.push(stack1.pop())
            f = true
          }
          case '>' => {
            if (f) stack1.push(stack2.pop())
            f = false
          }
          case '-' =>
          case x =>
            if (f) {
              if (stack1.size == 0) {
                stack2.push(x)
              } else {
                stack2.push(stack1.pop())
              }
            } else {
              if (stack2.size == 0) {
                stack1.push(x)
              } else {
                stack1.push(stack2.pop())
              }
            }
        }
      )
      while (stack1.size != 0) stack2.push(stack1.pop())
      println(stack2.toList.mkString(""))
      solve(sc, N - 1)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc, sc.nextInt)
  }
}
