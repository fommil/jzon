package zio.json.internal

// A data structure encoding a simple algorithm for Trie pruning: Given a list
// of strings, and a sequence of incoming characters, find the strings that
// match, by manually maintaining a bitset. Empty strings are not allowed.
//
// The caller should call "update" for each character in the string that is
// being tested for prefix matches, which allows the caller to avoid
// instantiating the full string and facilitates early exit. "first" and "exact"
// are used to test which result wins, with -1 meaning that no string matched.
//
// An internal bitset is maintained that holds a 1 in the bit corresponding to
// the index of every String that matches the prefixes that have been provided
// thus far, and a 0 if it failed.
//
// The number of strings that can be compared is limited to 63 as this allows
// the internal state to be storted in a 64 bit integer (the 0 state
// representing no matches). It would be possible to extend this to use
// BigInteger but it would incur extra overhead and it starts to get to the
// point where the performance benefit over materialising strings and searching
// an Array or Map becomes negligible.
final class StringMatrix(val xs: Array[String]) {
  require(xs.forall(_.nonEmpty))
  require(xs.nonEmpty)
  require(xs.length < 64)

  val width               = xs.length
  val height              = xs.map(_.length).max
  val lengths: Array[Int] = xs.map(_.length)
  val initial: Long       = (0 until width).foldLeft(0L)((bs, r) => bs | (1L << r))
  private val matrix: Array[Int] = {
    val m           = Array.fill[Int](width * height)(-1)
    var string: Int = 0
    while (string < width) {
      val s         = xs(string)
      val len       = s.length
      var char: Int = 0
      while (char < len) {
        m(width * char + string) = s.codePointAt(char)
        char += 1
      }
      string += 1
    }
    m
  }

  // must be called with increasing `char` (starting with bitset obtained from a
  // call to 'initial', char = 0)
  def update(bitset: Long, char: Int, c: Int): Long =
    if (char >= height) 0L    // too long
    else if (bitset == 0L) 0L // everybody lost
    else {
      var latest: Long = bitset
      val base: Int    = width * char

      if (bitset == initial) { // special case when it is dense since it is simple
        var string: Int = 0
        while (string < width) {
          if (matrix(base + string) != c)
            latest = latest ^ (1L << string)
          string += 1
        }
      } else {
        var remaining: Long = bitset
        while (remaining != 0L) {
          val string: Int = java.lang.Long.numberOfTrailingZeros(remaining)
          val bit: Long   = 1L << string
          if (matrix(base + string) != c)
            latest = latest ^ bit
          remaining = remaining ^ bit
        }
      }

      latest
    }

  // excludes entries that are not the given exact length
  def exact(bitset: Long, length: Int): Long =
    if (length > height) 0L // too long
    else {
      var latest: Long    = bitset
      var remaining: Long = bitset
      while (remaining != 0L) {
        val string: Int = java.lang.Long.numberOfTrailingZeros(remaining)
        val bit: Long   = 1L << string
        if (lengths(string) != length)
          latest = latest ^ bit
        remaining = remaining ^ bit
      }
      latest
    }

  def first(bitset: Long): Int =
    if (bitset == 0L) -1
    else java.lang.Long.numberOfTrailingZeros(bitset) // never returns 64
}
