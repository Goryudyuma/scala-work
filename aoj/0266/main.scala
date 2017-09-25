import java.util.Scanner

object Main {

  def initialize(): Map[(Char, Char), Char] = {
    Map(
      ('A', '0') -> 'X',
      ('A', '1') -> 'Y',
      ('X', '0') -> 'E',
      ('X', '1') -> 'Z',
      ('Z', '0') -> 'W',
      ('Z', '1') -> 'B',
      ('B', '0') -> 'Y',
      ('B', '1') -> 'X',
      ('Y', '0') -> 'X',
      ('Y', '1') -> 'E',
      ('W', '0') -> 'B',
      ('W', '1') -> 'Y',
      ('E', '0') -> 'E',
      ('E', '1') -> 'E'
    )
  }

  def check(S: String, now: Char, M: Map[(Char, Char), Char]): Char = {
    if (S.size == 0) (now) else (check(S.drop(1), M((now, S.charAt(0))), M))
  }

  def solve(sc: => Scanner, M: Map[(Char, Char), Char]): Unit = {
    val S = sc.next
    if (S != "#") {
      println(if (check(S, 'A', M) == 'B') ("Yes") else ("No"))
      solve(sc, M)
    }
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)

    solve(sc, initialize())
  }
}
