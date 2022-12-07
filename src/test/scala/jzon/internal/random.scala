// https://raw.githubusercontent.com/scalaprops/scalaprops
// License MIT, Copyright 2016 Kenji Yoshida
//
// Deterministic Random Number Generator (DRG) with a Mersenne Twister impl.
package jzon.internal

abstract class Rand {
  def nextLong: (Rand, Long)

  def nextInt: (Rand, Int) = nextIntFromNextLong

  protected[this] final def nextIntFromNextLong: (Rand, Int) = {
    val (r, n) = nextLong
    (r, (n >>> 32).toInt)
  }

  protected[this] final def nextLongFromNextInt: (Rand, Long) = {
    val (_, n1) = nextInt
    val (r, n2) = nextInt
    val x       = ((n1 & 0xFFFFFFFFL) << 32) | (n2 & 0xFFFFFFFFL)
    (r, x)
  }

  def nextDouble: (Rand, Double) = {
    val x       = nextInt
    val a: Long = (x._2.toLong & 0xFFFFFFFFL) >>> 5
    val y       = x._1.nextInt
    val b: Long = (y._2.toLong & 0xFFFFFFFFL) >>> 6
    val r       = (a * 67108864.0 + b) / 9007199254740992.0
    (y._1, r)
  }

  def chooseLong(from: Long, to: Long): (Rand, Long) =
    if (from == to) {
      (this.nextInt._1, from)
    } else {
      val min = math.min(from, to)
      val max = math.max(from, to)

      if (Int.MinValue <= min && max <= Int.MaxValue) {
        val (r, i) = choose(min.asInstanceOf[Int], max.asInstanceOf[Int])
        (r, i)
      } else {
        val diff: Long = (max: Long) - (min: Long)
        // `0 < diff` is necessary to check subtraction underflow.
        if (0 < diff && diff < Int.MaxValue) {
          val (r, i) = choose(0, diff.asInstanceOf[Int])
          (r, min + i)
        } else {
          @annotation.tailrec
          def loop(state: Rand): (Rand, Long) = {
            val next = state.nextLong
            if (min <= next._2 && next._2 <= max) {
              next
            } else if (0 < diff) {
              val x = (next._2 % (max - min + 1)) + min
              if (min <= x && x <= max) {
                (next._1, x)
              } else {
                loop(next._1)
              }
            } else {
              loop(next._1)
            }
          }
          loop(this)
        }
      }
    }

  def choose(from: Int, to: Int): (Rand, Int) =
    if (from == to) {
      (this.nextInt._1, from)
    } else {
      val min = math.min(from, to)
      val max = math.max(from, to)
      @annotation.tailrec
      def loop(state: Rand): (Rand, Int) = {
        val next = state.nextInt
        if (min <= next._2 && next._2 <= max) {
          next
        } else if (0 < (max - min)) {
          val x = (next._2 % (max - min + 2)) + min - 1
          if (min <= x && x <= max) {
            (next._1, x)
          } else {
            loop(next._1)
          }
        } else {
          loop(next._1)
        }
      }
      loop(this)
    }

}

private final class MersenneTwister64(
  val mt0: Array[Long],
  val mti0: Int
) extends Rand {
  import MersenneTwister64._

  def nextLong: (MersenneTwister64, Long) = {
    var mti = mti0
    var x   = 0L

    val mt1 = if (mti >= N) {
      val mt = mt0.clone()
      var kk = 0

      while (kk < N_M) {
        x = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + M) ^ (x >>> 1) ^ mag01(x)
        kk += 1
      }

      while (kk < N_1) {
        x = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + M_N) ^ (x >>> 1) ^ mag01(x)
        kk += 1
      }

      x = (mt(N_1) & UpperMask) | (mt(0) & LowerMask)
      mt(N_1) = mt(M_1) ^ (x >>> 1) ^ mag01(x)

      mti = 0
      mt
    } else {
      mt0
    }

    x = mt1(mti)
    mti += 1

    // Tempering
    x ^= (x >>> 29) & 0x5555555555555555L
    x ^= (x << 17) & 0x71D67FFFEDA60000L
    x ^= (x << 37) & 0xFFF7EEE000000000L
    x ^= (x >>> 43)

    (new MersenneTwister64(mt1, mti), x)
  }

  @inline def mag01(x: Long) =
    if ((x & 1) == 0) 0L else 0xB5026F5AA96619EL
}

object MersenneTwister64 {
  private final val UpperMask = 0xFFFFFFFF80000000L // = 0xFFFFFFFFFFFFFFFFL ^ Int.MinValue
  private final val LowerMask = 0x7FFFFFFFL // = Int.MinValue
  private final val N         = 312
  private final val M         = 156
  private final val N_M       = N - M
  private final val N_1       = N - 1
  private final val M_N       = M - N
  private final val M_1       = M - 1

  def apply(seed: Long): Rand = {
    val a = new Array[Long](N)
    a(0) = seed

    var i = 1
    while (i < N) {
      val x = a(i - 1)
      a(i) = 6364136223846793005L * (x ^ (x >>> 62)) + i
      i += 1
    }

    new MersenneTwister64(a, N + 1)
  }

}
