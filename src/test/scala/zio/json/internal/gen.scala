// https://raw.githubusercontent.com/scalaprops/scalaprops
// License MIT, Copyright 2016 Kenji Yoshida
//
// Data generator.
package zio.json.internal

import java.util.UUID
import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset.UTC

import scala.annotation._

final class Gen[A](val f: Rand => (Rand, A)) {
  def map[B](g: A => B): Gen[B] =
    Gen { r =>
      val (r0, a) = f(r)
      (r0, g(a))
    }

  def flatMap[B](g: A => Gen[B]): Gen[B] =
    Gen { r =>
      val (r0, a) = f(r)
      g(a).f(r0)
    }
}

object Gen {
  def apply[A](f: Rand => (Rand, A)): Gen[A] = new Gen(f)

  case class Config(seed: Long, count: Int)
  object Config {
    implicit val default: Config = Config(seed = 1337, count = 1000)
  }

  def prop[A](g: Gen[A])(f: A => Unit)(implicit c: Config): Unit =
    infinite(MersenneTwister64(c.seed), g).take(c.count).foreach { a =>
      // if we want to add minimisation / shrinking, this is where it would go:
      // take an implicit for each generator and then intercept failures and
      // retry until we get a pass. Try until all there is nowhere else to go.
      try f(a)
      catch {
        case t: Throwable =>
          var str = a.toString
          if (str.length > 100) str = str.take(100) + "..."
          println(s"PROPERTY FAILED ON: $str")
          throw t
      }
    }

  def prop[A1, A2](g1: Gen[A1], g2: Gen[A2])(f: (A1, A2) => Unit)(implicit c: Config): Unit =
    prop(tupled(g1, g2)) { case (a1, a2) => f(a1, a2) }

  def prop[A1, A2, A3](g1: Gen[A1], g2: Gen[A2], g3: Gen[A3])(f: (A1, A2, A3) => Unit)(implicit c: Config): Unit =
    prop(tupled(g1, g2, g3)) { case (a1, a2, a3) => f(a1, a2, a3) }

  def pure[A](a: A): Gen[A]          = Gen((_, a))
  def delay[A](a: => Gen[A]): Gen[A] = Gen(r => a.f(r))

  def infinite[A](r: Rand, g: Gen[A]): Iterator[A] =
    new Iterator[A] {
      private[this] var rand = r
      override def next(): A = {
        val x = g.f(rand)
        rand = x._1
        x._2
      }
      override def hasNext = true
    }

  def tupled[A1, A2](g1: Gen[A1], g2: Gen[A2]) =
    for {
      a1 <- g1
      a2 <- g2
    } yield (a1, a2)

  def tupled[A1, A2, A3](g1: Gen[A1], g2: Gen[A2], g3: Gen[A3]) =
    for {
      a1 <- g1
      a2 <- g2
      a3 <- g3
    } yield (a1, a2, a3)

  def oneOf[A](head: Gen[A], tail: Gen[A]*): Gen[A] = {
    val all = head +: tail
    for {
      idx    <- choose(0, all.length - 1)
      choice <- all(idx)
    } yield choice
  }

  def sampleChar(cs: Array[Char]): Gen[Char] =
    Gen { (r: Rand) =>
      val (random, long) = r.nextLong
      val i              = ((long & Long.MaxValue) % cs.length).toInt
      (random, cs(i))
    }

  val hexChar: Gen[Char] =
    sampleChar("0123456789abcdef0123456789ABCDEF".toArray)

  val alphaChar: Gen[Char] =
    sampleChar("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".toArray)

  val alphanumericChar: Gen[Char] =
    sampleChar("0123456789abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".toArray)

  val asciiChar: Gen[Char] =
    sampleChar((0.toChar to 127.toChar).toArray)

  // does not support surrogate pairs
  def string(g: Gen[Char], maxSize: Int = 1024): Gen[String] =
    nel(g, maxSize).map(_.mkString)

  def alpha(maxSize: Int = 1024): Gen[String]        = string(alphaChar, maxSize)
  def alphanumeric(maxSize: Int = 1024): Gen[String] = string(alphanumericChar, maxSize)
  def ascii(maxSize: Int = 1024): Gen[String]        = string(asciiChar, maxSize)

  // samples from all valid utf-8 values, including those that are represented
  // as multiple Chars (surrogate pairs) in Java's utf-16 encoding.
  def unicode(maxSize: Int = 1024): Gen[String] = {
    val chars = choose(0, 0x10FFFF).map { cp =>
      if (Character.isBmpCodePoint(cp)) {
        if (cp < 0xD800) List(cp.toChar)
        else List('?')
      } else List(Character.highSurrogate(cp), Character.lowSurrogate(cp))
    }
    nel(chars, maxSize).map(_.flatten.mkString)
  }

  val long: Gen[Long]       = Gen(r => r.nextLong)
  val int: Gen[Int]         = long.map(_.toInt)
  val short: Gen[Short]     = long.map(_.toShort)
  val byte: Gen[Byte]       = long.map(_.toByte)
  val boolean: Gen[Boolean] = long.map(_ % 2 == 0)

  // chars that are valid on their own (not surrogates)
  val char: Gen[Char] = Gen.choose(0, 0xD7FF).map(_.toChar)

  val double: Gen[Double] = long.map(java.lang.Double.longBitsToDouble(_))
  val float: Gen[Float]   = double.map(_.toFloat)

  def biginteger(maxBitSize: Int): Gen[java.math.BigInteger] =
    nel(byte, maxBitSize / 8).map(bytes => new java.math.BigInteger(bytes.toArray))

  def bigdecimal(maxBitSize: Int): Gen[java.math.BigDecimal] =
    for {
      bigint <- biginteger(maxBitSize)
      scale  <- int
    } yield new java.math.BigDecimal(bigint, scale)

  def chooseLong(from: Long, to: Long): Gen[Long] =
    Gen(r => r.chooseLong(from, to))

  def choose(from: Int, to: Int): Gen[Int] =
    Gen(r => r.choose(from, to))

  def list[A](g: Gen[A], maxSize: Int): Gen[List[A]] =
    choose(0, maxSize).flatMap(n => listOfN(g, n))

  def nel[A](g: Gen[A], maxSize: Int): Gen[List[A]] =
    choose(1, maxSize).flatMap(n => listOfN(g, n))

  def listOfN[A](g: Gen[A], n: Int): Gen[List[A]] = {
    @tailrec def loop(i: Int, next: Rand, acc: List[A]): (Rand, List[A]) =
      if (i < n) {
        val r = g.f(next)
        loop(i + 1, r._1, r._2 :: acc)
      } else {
        (next, acc.reverse)
      }
    Gen(r => loop(0, r, List.empty[A]))
  }

  def elements[A](a: A, as: A*): Gen[A] = {
    val xs = (a +: as).toArray[Any]
    choose(0, as.length).map(xs(_).asInstanceOf[A])
  }

  def frequency_[A](g: (A, Long), gs: (A, Long)*): Gen[A] = {
    def lift(t: (A, Long)): (Gen[A], Long) = (pure(t._1), t._2)
    frequency(lift(g), gs.map(lift): _*)
  }

  def frequency[A](g: (Gen[A], Long), gs: (Gen[A], Long)*): Gen[A] = {
    val tail = gs.toList
    chooseLong(1, tail.foldLeft(g._2)(_ + _._2)).flatMap(i => pick0(i, g, tail))
  }
  @tailrec private def pick0[A](n: Long, head: (A, Long), tail: List[(A, Long)]): A = {
    val k = head._2
    if (n <= k) head._1
    else pick0(n - k, tail.head, tail.tail)
  }

  val uuid: Gen[UUID] = for {
    high <- Gen.long
    low  <- Gen.long
  } yield new UUID(high, low)

  def instant(min: Instant, max: Instant): Gen[Instant] = {
    val min_seconds = min.getEpochSecond
    val max_seconds = max.getEpochSecond
    for {
      seconds   <- Gen.chooseLong(min_seconds, max_seconds)
      min_nanos = if (seconds == min_seconds) min.getNano else 0
      max_nanos = if (seconds == max_seconds) max.getNano else 999999999
      nanos     <- Gen.choose(min_nanos, max_nanos)
    } yield Instant.ofEpochSecond(seconds, nanos)
  }

  def localDateTime(min: LocalDateTime, max: LocalDateTime): Gen[LocalDateTime] =
    instant(min.toInstant(UTC), max.toInstant(UTC)).map(LocalDateTime.ofInstant(_, UTC))

}
