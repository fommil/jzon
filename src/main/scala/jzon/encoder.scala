package jzon

import scala.annotation._
import scala.collection.immutable

import jzon.internal._

// convenient `.toJson` syntax
object syntax {
  implicit final class EncoderOps[A](private val a: A) extends AnyVal {
    def toJson(implicit A: Encoder[A]): String = A.toJson(a, None)

    // Jon Pretty's better looking brother, but a bit slower
    def toJsonPretty(implicit A: Encoder[A]): String = A.toJson(a, Some(0))
  }
}

trait Encoder[A] { self =>
  def toJson(a: A, indent: Option[Int]): String = {
    val writer = new FastStringWriter(64)
    unsafeEncode(a, indent, writer)
    writer.toString
  }

  // scalaz-deriving style Contravariant combinators
  final def narrow[B <: A]: Encoder[B] = self.asInstanceOf[Encoder[B]]
  final def contramap[B](f: B => A): Encoder[B] = new Encoder[B] {
    override def unsafeEncode(b: B, indent: Option[Int], out: java.io.Writer): Unit =
      self.unsafeEncode(f(b), indent, out)
    override def isNothing(b: B): Boolean = self.isNothing(f(b))
  }
  final def xmap[B](f: A => B, g: B => A): Encoder[B] = contramap(g)

  def unsafeEncode(a: A, indent: Option[Int], out: java.io.Writer): Unit

  // override and return `true` when this value may be skipped from JSON Objects
  def isNothing(a: A): Boolean = false
}

object Encoder extends EncoderGenerated with EncoderLowPriority1 {
  def apply[A](implicit a: Encoder[A]): Encoder[A] = a

  def derived[A, B](implicit S: shapely.Shapely[A, B], B: Encoder[B]): Encoder[A] = B.contramap(S.to)

  implicit val charseq: Encoder[CharSequence] = new Encoder[CharSequence] {
    override def unsafeEncode(a: CharSequence, indent: Option[Int], out: java.io.Writer): Unit = {
      out.write('"')
      var i   = 0
      val len = a.length
      while (i < len) {
        (a.charAt(i): @switch) match {
          case '"'  => out.write("\\\"")
          case '\\' => out.write("\\\\")
          case '\b' => out.write("\\b")
          case '\f' => out.write("\\f")
          case '\n' => out.write("\\n")
          case '\r' => out.write("\\r")
          case '\t' => out.write("\\t")
          case c =>
            if (c < ' ') out.write("\\u%04x".format(c.toInt))
            else out.write(c)
        }
        i += 1
      }
      out.write('"')
    }
  }
  implicit val string: Encoder[String] = charseq.narrow

  private[this] def explicit[A](f: A => String): Encoder[A] = new Encoder[A] {
    def unsafeEncode(a: A, indent: Option[Int], out: java.io.Writer): Unit = out.write(f(a))
  }
  implicit val boolean: Encoder[Boolean] = explicit(_.toString)
  implicit val char: Encoder[Char]       = string.contramap(_.toString)
  implicit val symbol: Encoder[Symbol]   = string.contramap(_.name)

  implicit val byte: Encoder[Byte]                       = explicit(_.toString)
  implicit val short: Encoder[Short]                     = explicit(_.toString)
  implicit val int: Encoder[Int]                         = explicit(_.toString)
  implicit val long: Encoder[Long]                       = explicit(_.toString)
  implicit val biginteger: Encoder[java.math.BigInteger] = explicit(_.toString)
  implicit val double: Encoder[Double] = explicit { n =>
    if (n.isNaN || n.isInfinite) s""""$n""""
    else n.toString
  }
  implicit val float: Encoder[Float]                     = double.contramap(_.toDouble)
  implicit val bigdecimal: Encoder[java.math.BigDecimal] = explicit(_.toString)

  implicit def option[A](implicit A: Encoder[A]): Encoder[Option[A]] = new Encoder[Option[A]] {
    def unsafeEncode(oa: Option[A], indent: Option[Int], out: java.io.Writer): Unit = oa match {
      case None    => out.write("null")
      case Some(a) => A.unsafeEncode(a, indent, out)
    }
    override def isNothing(a: Option[A]): Boolean = a.isEmpty
  }

  def bump(indent: Option[Int]): Option[Int] = indent match {
    case None    => None
    case Some(i) => Some(i + 1)
  }
  def pad(indent: Option[Int], out: java.io.Writer): Unit =
    indent.foreach(i => out.write("\n" + (" " * 2 * i)))

  implicit def either[A, B](implicit A: Encoder[A], B: Encoder[B]): Encoder[Either[A, B]] = new Encoder[Either[A, B]] {
    def unsafeEncode(eab: Either[A, B], indent: Option[Int], out: java.io.Writer): Unit = {
      out.write("{")
      val indent_ = bump(indent)
      pad(indent_, out)
      eab match {
        case Left(a) =>
          out.write("\"Left\"")
          if (indent.isEmpty) out.write(":")
          else out.write(" : ")
          A.unsafeEncode(a, indent_, out)
        case Right(b) =>
          out.write("\"Right\"")
          if (indent.isEmpty) out.write(":")
          else out.write(" : ")
          B.unsafeEncode(b, indent_, out)
      }
      pad(indent, out)
      out.write("}")
    }
  }
}

private[jzon] trait EncoderLowPriority1 {
  this: Encoder.type =>

  implicit def list[A: Encoder]: Encoder[List[A]]     = seq[A].narrow
  implicit def vector[A: Encoder]: Encoder[Vector[A]] = seq[A].narrow
  implicit def seq[A](implicit A: Encoder[A]): Encoder[Seq[A]] = new Encoder[Seq[A]] {
    def unsafeEncode(as: Seq[A], indent: Option[Int], out: java.io.Writer): Unit = {
      out.write("[")
      var first = true
      as.foreach { a =>
        if (first) first = false
        else if (indent.isEmpty) out.write(",")
        else out.write(", ")
        A.unsafeEncode(a, indent, out)
      }
      out.write("]")
    }
  }

  // not implicit because this overlaps with encoders for lists of tuples
  def keylist[K, A](
    implicit
    K: FieldEncoder[K],
    A: Encoder[A]
  ): Encoder[List[(K, A)]] = new Encoder[List[(K, A)]] {
    def unsafeEncode(kvs: List[(K, A)], indent: Option[Int], out: java.io.Writer): Unit = {
      if (kvs.isEmpty) return out.write("{}")

      out.write("{")
      val indent_ = bump(indent)
      pad(indent_, out)
      var first = true
      kvs.foreach {
        case (k, a) =>
          if (!A.isNothing(a)) {
            if (first)
              first = false
            else if (indent.isEmpty)
              out.write(",")
            else {
              out.write(",")
              pad(indent_, out)
            }

            string.unsafeEncode(K.unsafeEncodeField(k), indent_, out)
            if (indent.isEmpty) out.write(":")
            else out.write(" : ")
            A.unsafeEncode(a, indent_, out)
          }
      }
      pad(indent, out)
      out.write("}")
    }
  }

  implicit def sortedmap[K: FieldEncoder, V: Encoder]: Encoder[collection.SortedMap[K, V]] =
    keylist[K, V].contramap(_.toList)
  implicit def map[K: FieldEncoder, V: Encoder]: Encoder[Map[K, V]] =
    keylist[K, V].contramap(_.toList)
  implicit def hashmap[K: FieldEncoder, V: Encoder]: Encoder[immutable.HashMap[K, V]] =
    keylist[K, V].contramap(_.toList)
  implicit def set[A: Encoder]: Encoder[Set[A]] =
    list[A].contramap(_.toList)
  implicit def hashset[A: Encoder]: Encoder[immutable.HashSet[A]] =
    list[A].contramap(_.toList)
  implicit def sortedset[A: Encoder]: Encoder[immutable.SortedSet[A]] =
    list[A].contramap(_.toList)

}

/** When encoding a JSON Object, we only allow keys that implement this interface. */
trait FieldEncoder[A] { self =>

  final def narrow[B <: A]: FieldEncoder[B] = self.asInstanceOf[FieldEncoder[B]]
  final def contramap[B](f: B => A): FieldEncoder[B] = new FieldEncoder[B] {
    override def unsafeEncodeField(in: B): String = self.unsafeEncodeField(f(in))
  }
  final def xmap[B](f: A => B, g: B => A): FieldEncoder[B] = contramap(g)

  def unsafeEncodeField(in: A): String
}
object FieldEncoder {
  implicit val string: FieldEncoder[String] = new FieldEncoder[String] {
    def unsafeEncodeField(in: String): String = in
  }
}

// Common code that is mixed into all the generated CaseClass encoders.
private[jzon] abstract class CaseClassEncoder[A, CC <: shapely.CaseClass[A]](M: shapely.Meta[A]) extends Encoder[CC] {
  val names: Array[String] = M.fieldAnnotations
    .zip(M.fieldNames)
    .map {
      case (a, n) => a.collectFirst { case field(name) => name }.getOrElse(n)
    }
    .toArray

  def isNothing(cc: CC, i: Int): Boolean
  def unsafeEncode(cc: CC, indent: Option[Int], out: java.io.Writer, i: Int): Unit

  final override def unsafeEncode(cc: CC, indent: Option[Int], out: java.io.Writer): Unit = {
    var i = 0
    out.write("{")
    val indent_ = Encoder.bump(indent)
    Encoder.pad(indent_, out)

    while (i < names.length) {
      if (!isNothing(cc, i)) {
        if (i > 0) {
          if (indent.isEmpty) out.write(",")
          else {
            out.write(",")
            Encoder.pad(indent_, out)
          }
        }
        Encoder.string.unsafeEncode(names(i), indent_, out)
        if (indent.isEmpty) out.write(":")
        else out.write(" : ")
        unsafeEncode(cc, indent_, out, i)
      }
      i += 1
    }
    Encoder.pad(indent, out)
    out.write("}")
  }
}

private[jzon] abstract class SealedTraitEncoder[A, ST <: shapely.SealedTrait[A]](subs: Array[shapely.Meta[_]])
    extends Encoder[ST] {
  val names: Array[String] = subs.map(m => m.annotations.collectFirst { case hint(name) => name }.getOrElse(m.name))
  val matrix: StringMatrix = new StringMatrix(names)

  def unsafeEncodeValue(st: ST, indent: Option[Int], out: java.io.Writer): Unit

  final override def unsafeEncode(st: ST, indent: Option[Int], out: java.io.Writer): Unit = {
    out.write("{")
    val indent_ = Encoder.bump(indent)
    Encoder.pad(indent_, out)
    Encoder.string.unsafeEncode(names(st.index), indent_, out)
    if (indent.isEmpty) out.write(":")
    else out.write(" : ")
    unsafeEncodeValue(st, indent_, out)
    Encoder.pad(indent, out)
    out.write("}")
  }
}

private[jzon] abstract class SealedTraitDiscrimEncoder[A, ST <: shapely.SealedTrait[A]](
  subs: Array[shapely.Meta[_]],
  hintfield: String
) extends Encoder[ST] {
  val names: Array[String] = subs.map(m => m.annotations.collectFirst { case hint(name) => name }.getOrElse(m.name))
  val matrix: StringMatrix = new StringMatrix(names)

  def unsafeEncodeValue(st: ST, indent: Option[Int], out: java.io.Writer): Unit

  final override def unsafeEncode(st: ST, indent: Option[Int], out: java.io.Writer): Unit = {
    out.write("{")
    val indent_ = Encoder.bump(indent)
    Encoder.pad(indent_, out)
    Encoder.string.unsafeEncode(hintfield, indent_, out)
    if (indent.isEmpty) out.write(":")
    else out.write(" : ")
    Encoder.string.unsafeEncode(names(st.index), indent_, out)

    // whitespace is always off by 2 spaces at the end, probably not worth fixing
    val intermediate = new NestedWriter(out, indent_)
    unsafeEncodeValue(st, indent, intermediate)
  }

}

// intercepts the first `{` of a nested writer and discards it. We also need to
// inject a `,` unless an empty object `{}` has been written.
private final class NestedWriter(out: java.io.Writer, indent: Option[Int]) extends java.io.Writer {
  def close(): Unit               = out.close()
  def flush(): Unit               = out.flush()
  private[this] var first, second = true
  def write(cs: Array[Char], from: Int, len: Int): Unit =
    if (first || second) {
      var i = 0
      while (i < len) {
        val c = cs(from + i)
        if (c == ' ' || c == '\n') {} else if (first && c == '{') {
          first = false
        } else if (second) {
          second = false
          if (c != '}') {
            out.append(",")
            Encoder.pad(indent, out)
          }
          return out.write(cs, from + i, len - i)
        }
        i += 1
      }
    } else out.write(cs, from, len)
}
