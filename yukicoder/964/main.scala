//import java.util.Scanner

import java.io._
import java.nio.file.Files._
import java.nio.file.Path
import java.util.StringTokenizer

import scala.collection.immutable._
import scala.io.Codec

/**
  * Scala implementation of a faster java.util.Scanner
  * See: http://codeforces.com/blog/entry/7018
  */

class Scanner(reader: LineNumberReader) extends Iterable[String] with AutoCloseable {
  def this(reader: BufferedReader) = this(new LineNumberReader(reader))

  def this(reader: Reader) = this(new BufferedReader(reader))

  def this(inputStream: InputStream)(implicit codec: Codec) = this(new InputStreamReader(inputStream, codec.charSet))

  def this(path: Path)(implicit codec: Codec) = this(newBufferedReader(path, codec.charSet))

  def this(file: File)(implicit codec: Codec) = this(file.toPath)(codec)

  def this(str: String) = this(new StringReader(str))

  override def iterator: Iterator[String] = for {
    line <- Iterator.continually(reader.readLine()).takeWhile(_ != null)
    tokenizer = new StringTokenizer(line)
    tokens <- Iterator.continually(tokenizer).takeWhile(_.hasMoreTokens)
  } yield tokens.nextToken()

  private[this] var current = iterator

  def hasNext: Boolean = current.hasNext

  @inline def next(): String = current.next()

  /**
    * This is different from Java's scanner.nextLine
    * The Java one is a misnomer since it actually travel to end of current line
    * This one actually does fetch the next line
    */
  def nextLine(): String = {
    val line = reader.readLine()
    current = iterator
    line
  }

  def lineNumber: Int = reader.getLineNumber

  def nextString(): String = next()

  def nextBoolean(): Boolean = next().toBoolean

  def nextByte(radix: Int = 10): Byte = java.lang.Byte.parseByte(next(), radix)

  def nextShort(radix: Int = 10): Short = java.lang.Short.parseShort(next(), radix)

  def nextInt(radix: Int = 10): Int = java.lang.Integer.parseInt(next(), radix)

  def nextLong(radix: Int = 10): Long = java.lang.Long.parseLong(next(), radix)

  def nextBigInt(radix: Int = 10): BigInt = BigInt(next(), radix)

  def nextFloat(): Float = next().toFloat

  def nextDouble(): Double = next().toDouble

  def nextBigDecimal(): BigDecimal = BigDecimal(next())

  override def close(): Unit = reader.close()
}


class IUnionFind(val size: Int) {

  private case class Node(var parent: Option[Int], var treeSize: Int)

  private val nodes = Array.fill[Node](size)(Node(None, treeSize = 1))

  def union(t1: Int, t2: Int): IUnionFind = {
    if (t1 == t2) return this

    val root1 = root(t1)
    val root2 = root(t2)
    if (root1 == root2) return this

    val node1 = nodes(root1)
    val node2 = nodes(root2)

    if (node1.treeSize < node2.treeSize) {
      node1.parent = Some(t2)
      node2.treeSize += node1.treeSize
    } else {
      node2.parent = Some(t1)
      node1.treeSize += node2.treeSize
    }
    this
  }

  def connected(t1: Int, t2: Int): Boolean = t1 == t2 || root(t1) == root(t2)

  def root(t: Int): Int = nodes(t).parent match {
    case None => t
    case Some(p) =>
      nodes(t).parent = Some(root(p))
      nodes(t).parent.get
  }
}


object Main {
  def solve(sc: => Scanner): Unit = {
    val N = sc.nextInt()
    println(
      if (N == 1) 1
      else (0 until N).map(i => i.toString.repeat(N)).reverse.mkString("")
    )
  }

  val mod: Long = (1e9 + 7).toLong

  @scala.annotation.tailrec
  def recursive(N: Long, M: Long, A: Long): Long = {
    if (N == 0) A else recursive(N - 1, M, A * 10 + M)
  }

  implicit class implicitInt(val N: Int) {
    def times[B](function: Int => B): IndexedSeq[B] = (0 until N).map(function)
  }

  def calc(input: (Long, Long, Long, Long, Long, Long)): Long = {
    input match {
      case (a, b, c, x, y, z) => {
        var ans: Long = Long.MaxValue
        if (b >= 3 && (a >= 2 || c >= 2)) {
          ans = Math.min(ans, Math.max(a - (b - 1), 0) * x + Math.max(c - Math.min(a - 1, b - 2), 0) * z)
          ans = Math.min(ans, Math.max(a - Math.min(c - 1, b - 2), 0) * x + Math.max(c - (b - 1), 0) * z)
        }
        //        println(ans)
        if (b >= 2 && (a >= b && c >= b + 1)) {
          ans = Math.min(ans, y + Math.max(0, a - (c - 1)) * x)
        }
        //        println(ans)
        if (b >= 2 && (a >= b + 1 && c >= b)) {
          ans = Math.min(ans, y + Math.max(0, c - (a - 1)) * z)
        }
        //        println(ans)
        if (a >= 2 && c >= 2)
          if (a != c)
            ans = Math.min(ans, Math.max(0, b - (Math.min(a, c) - 1)) * y)
          else if (a >= 3 && c >= 3)
            ans = Math.min(ans, Math.max(0, b - (Math.min(a, c) - 2)) * y + Math.min(x, z))
        //        println(ans)
        if (ans == Long.MaxValue) ans = -1
        ans
      }
    }
  }

  def time(s: String): Int =
    s.substring(0, 2).toInt * 60 + s.substring(3, 5).toInt

  def getPermutation(begin: Long = 0): Stream[Long] =
    Stream.cons(begin, getPermutation(begin + 1))

  def getFibonacci(prevprev: Long = 0, prev: Long = 1): Stream[Long] =
    Stream.cons(prevprev, getFibonacci(prev, prevprev + prev))

  var memo: Map[(Long, Long), Long] = Map[(Long, Long), Long]()

  @scala.annotation.tailrec
  def recursive2(X: Set[Long], Y: Stream[Long]): Long = if (X.contains(Y.head)) X.size else recursive2(X + Y.head, Y.tail)

  def check(i: Int, X: String): Long = {
    if (X == "") 0 else check(i, X.tail) * i + X.head.toString.toLong
  }

  def shift(n: Long): Long = {
    if (n == 0) 0
    else if (n == 1) 1
    else shift(n - 1) << 1
  }

  def unShift(n: Long): Long = {
    if (n == 0) 0
    else unShift(n >> 1) + 1
  }

  @scala.annotation.tailrec
  def gcd(i: Long, j: Long): Long = {
    if (i < j) gcd(j, i)
    else if (j == 0) i
    else gcd(j, i % j)
  }

  def primeFactors(i: Long): List[Long] = primeFactors_(i, 1).sorted


  def primeFactors_(i: Long, j: Long): List[Long] = {
    if (j * j > i) List.empty else if (i % j == 0) primeFactors_(i, j + 1) ++ List[Long](j, i / j) else primeFactors_(i, j + 1)
  }

  def main(args: Array[String]): Unit = {
    val sc: Scanner = new Scanner(System.in)
    solve(sc)
  }
}

object Util {
  def getPermutation(begin: Long = 0): Stream[Long] =
    Stream.cons(begin, getPermutation(begin + 1))

  def getPrimeList: Stream[Long] =
    getPrimeListRecursive(getPermutation(begin = 2))

  private def getPrimeListRecursive(A: Stream[Long]): Stream[Long] =
    Stream.cons(A.head, getPrimeListRecursive(A.tail.filter(_ % A.head != 0)))

  def fib(a: Long = 0, b: Long = 1, mod: Long = Long.MaxValue): Stream[Long] = a #:: fib(b % mod, (a + b) % mod, mod)
}

object ArabicRoman {

  type =?>[A, B] = PartialFunction[A, B]

  private val codeTable: List[(Int, String)] = List(
    (1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), (100, "C"),
    (90, "XC"), (50, "L"), (40, "XL"), (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I"))

  val arabicToRoman: Int =?> String = {
    case src if src >= 1 && src <= 3999 =>

      @scala.annotation.tailrec
      def convert(left: Int, cont: String = "", code: List[(Int, String)] = codeTable): String = {
        val (unitVal, unitChar) = code.head
        left - unitVal match {
          case n if n == 0 => cont + unitChar
          case n if n > 0 => convert(n, cont + unitChar, code)
          case _ => convert(left, cont, code.tail)
        }
      }

      convert(src)
  }

  val romanToArabic: String =?> Int = {
    case src if Option(src).exists { s => {
      s.nonEmpty && """[^MDCLXVI]""".r.findFirstMatchIn(s.toUpperCase).isEmpty
    }
    } =>

      @scala.annotation.tailrec
      def convert(left: String, cont: Int = 0, code: List[(Int, String)] = codeTable): Int = {
        val (unitVal, unitChar) = code.head
        left.splitAt(unitChar.length) match {
          case ("", _) => cont
          case (`unitChar`, tail) => convert(tail, cont + unitVal, code)
          case _ => convert(left, cont, code.tail)
        }
      }

      convert(src.toUpperCase())
  }
}

object PowMod {
  def naivePowMod(a: Long, k: Long, m: Long): Long = {
    var t: Long = 1
    val aMod: Long = a % m

    for (_ <- 1L to k) {
      t = ((t % m) * aMod) % m
    }
    t.toInt
  }

  def powMod(a: Long, k: Long, m: Long): Long = {
    if (k == 0) 1
    else {
      val t = powMod(a, k / 2, m)
      if ((k % 2) == 0) (t * t) % m else (((t * t) % m) * a) % m
    }
  }
}
