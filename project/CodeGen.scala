object CodeGen {
  val product_arity = 64
  val sum_arity     = 64

  def encoders: String = {
    val caseclasses = (1 to product_arity).map { i =>
      val tparams       = (1 to i).map(p => s"A$p").mkString(", ")
      val implicits     = (1 to i).map(p => s"A$p: Lazy[Encoder[A$p]]").mkString(", ")
      val isNothings    = (1 to i).map(p => s"case ${p - 1} => A${p}.value.isNothing(cc._${p})")
      val unsafeEncodes = (1 to i).map(p => s"case ${p - 1} => A${p}.value.unsafeEncode(cc._${p}, indent, out)")
      val CC            = s"CaseClass$i[A, $tparams]"

      s"""  implicit def caseclass$i[A, $tparams](implicit M: Meta[A], $implicits): Encoder[$CC] =
         |    new CaseClassEncoder[A, $CC](M) {
         |      override def isNothing(cc: $CC, i: Int): Boolean = (i: @switch) match {
         |        ${isNothings.mkString("\n        ")}
         |      }
         |      override def unsafeEncode(cc: $CC, indent: Option[Int], out: java.io.Writer, i: Int): Unit = (i: @switch) match {
         |        ${unsafeEncodes.mkString("\n        ")}
         |      }
         |    }""".stripMargin
    }

    val sealedtraits = (1 to sum_arity).map { i =>
      val tparams       = (1 to i).map(p => s"A$p <: A").mkString(", ")
      val tparams_      = (1 to i).map(p => s"A$p").mkString(", ")
      val implicits     = (1 to i).map(p => s"M$p: Meta[A$p], A$p: Lazy[Encoder[A$p]]").mkString(", ")
      val unsafeEncodes = (1 to i).map(p => s"case ${p - 1} => A${p}.value.unsafeEncode(cc._${p}, indent, out)")
      val ST            = s"SealedTrait$i[A, $tparams_]"
      val Ms            = (1 to i).map(p => s"M$p").mkString(", ")
      val work          = (1 to i).map(p => s"case SealedTrait._${p}(v) => A${p}.value.unsafeEncode(v, indent, out)")

      s"""  implicit def sealedtrait$i[A, $tparams](implicit M: Meta[A], $implicits): Encoder[$ST] = {
         |    def work(st: $ST, indent: Option[Int], out: java.io.Writer): Unit = st match {
         |      ${work.mkString("\n      ")}
         |    }
         |    M.annotations.collectFirst { case discriminator(n) => n } match {
         |      case None => new SealedTraitEncoder[A, $ST](Array($Ms)) {
         |        override def unsafeEncodeValue(st: $ST, indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
         |      }
         |      case Some(hintfield) => new SealedTraitDiscrimEncoder[A, $ST](Array($Ms), hintfield) {
         |        override def unsafeEncodeValue(st: $ST, indent: Option[Int], out: java.io.Writer): Unit = work(st, indent, out)
         |      }
         |    }
         |  }""".stripMargin

    }

    val tuples = (1 to 22).map { i =>
      val tparams   = (1 to i).map(p => s"A$p").mkString(", ")
      val implicits = (1 to i).map(p => s"A$p: Encoder[A$p]").mkString(", ")
      val work = (1 to i)
        .map(p => s"A$p.unsafeEncode(t._$p, indent, out)")
        .mkString("\n        if (indent.isEmpty) out.write(\",\") else out.write(\", \")\n        ")

      s"""  implicit def tuple${i}[$tparams](implicit $implicits): Encoder[Tuple${i}[$tparams]] =
         |    new Encoder[Tuple${i}[$tparams]] {
         |      override def unsafeEncode(t: Tuple${i}[$tparams], indent: Option[Int], out: java.io.Writer): Unit = {
         |        out.write("[")
         |        $work
         |        out.write("]")
         |      }
         |    }""".stripMargin
    }

    s"""package zio.json
       |
       |import scala.annotation._
       |import shapely._
       |
       |private[json] trait EncoderGenerated { this: Encoder.type =>
       |  implicit def caseclass0[A]: Encoder[CaseClass0[A]] = new Encoder[CaseClass0[A]] {
       |    override def unsafeEncode(a: CaseClass0[A], indent: Option[Int], out: java.io.Writer): Unit = out.write("{}")
       |  }
       |
       |  ${caseclasses.mkString("\n\n  ")}
       |  ${sealedtraits.mkString("\n\n  ")}
       |  ${tuples.mkString("\n\n  ")}
       |}""".stripMargin
  }

  def decoders: String = {
    val caseclasses = (1 to product_arity).map { i =>
      val tparams   = (1 to i).map(p => s"A$p").mkString(", ")
      val implicits = (1 to i).map(p => s"A$p: Lazy[Decoder[A$p]]").mkString(", ")
      val tcs       = (1 to i).map(p => s"A$p").mkString(", ")
      val cons      = (1 to i).map(p => s"ps(${p - 1}).asInstanceOf[A$p]").mkString(", ")

      s"""  implicit def caseclass$i[A, $tparams](implicit M: Meta[A], $implicits): Decoder[CaseClass$i[A, $tparams]] = {
         |    new CaseClassDecoder[A, CaseClass${i}[A, $tparams]](M) {
         |      override lazy val tcs = Array($tcs).map(_.value.widen[Any]).toArray
         |      override def cons(ps: Array[Any]) = CaseClass$i($cons)
         |    }
         |  }""".stripMargin
    }

    val sealedtraits = (1 to sum_arity).map { i =>
      val tparams   = (1 to i).map(p => s"A$p <: A").mkString(", ")
      val tparams_  = (1 to i).map(p => s"A$p").mkString(", ")
      val implicits = (1 to i).map(p => s"A$p: Lazy[Decoder[A$p]], M$p: Meta[A$p]").mkString(", ")
      val instances = (1 to i).map(p => s"A${p}.value.map(SealedTrait._${p}(_))").mkString(", ")
      val metas     = (1 to i).map(p => s"M$p").mkString(", ")

      s"""  implicit def sealedtrait$i[A, $tparams](implicit M: Meta[A], $implicits): Decoder[SealedTrait$i[A, $tparams_]] = {
         |    def instances: Array[Decoder[SealedTrait$i[A, $tparams_]]] = Array($instances)
         |    val metas: Array[Meta[_]] = Array($metas)
         |    M.annotations.collectFirst { case discriminator(n) => n } match {
         |      case None =>
         |        new SealedTraitDecoder[A, SealedTrait$i[A, $tparams_]](metas) {
         |          override lazy val tcs = instances
         |        }
         |      case Some(discrim) =>
         |        new SealedTraitDiscrimDecoder[A, SealedTrait$i[A, $tparams_]](metas, discrim) {
         |          override lazy val tcs = instances
         |        }
         |    }
         |  }
         |""".stripMargin
    }

    val tuples = (1 to 22).map { i =>
      val tparams   = (1 to i).map(p => s"A$p").mkString(", ")
      val implicits = (1 to i).map(p => s"A$p: Decoder[A$p]").mkString(", ")
      val work = (1 to i)
        .map(p => s"val a$p = A$p.unsafeDecode(traces($p) :: trace, in)")
        .mkString("\n        Lexer.char(trace, in, ',')\n        ")
      val returns = (1 to i).map(p => s"a$p").mkString(", ")

      s"""  implicit def tuple${i}[$tparams](implicit $implicits): Decoder[Tuple${i}[$tparams]] =
         |    new Decoder[Tuple${i}[$tparams]] {
         |      val traces: Array[JsonError] = (0 to $i).map(JsonError.ArrayAccess(_)).toArray
         |      override def unsafeDecode(trace: List[JsonError], in: RetractReader): Tuple${i}[$tparams] = {
         |        Lexer.char(trace, in, '[')
         |        $work
         |        Lexer.char(trace, in, ']')
         |        Tuple${i}($returns)
         |      }
         |    }""".stripMargin
    }

    s"""package zio.json
       |
       |import shapely._
       |import zio.json.internal._
       |
       |private[json] trait DecoderGenerated { this: Decoder.type =>
       |
       |  implicit def caseclass0[A](implicit M: Meta[A]): Decoder[CaseClass0[A]] = new Decoder[CaseClass0[A]] {
       |    val no_extra = M.annotations.collectFirst { case _: no_extra_fields => () }.isDefined
       |    override def unsafeDecode(trace: List[JsonError], in: RetractReader): CaseClass0[A] = {
       |      if (no_extra) {
       |        Lexer.char(trace, in, '{')
       |        Lexer.char(trace, in, '}')
       |      } else {
       |        Lexer.skipValue(trace, in, null)
       |      }
       |      CaseClass0()
       |    }
       |  }
       |
       |  ${caseclasses.mkString("\n\n  ")}
       |  ${sealedtraits.mkString("\n\n  ")}
       |  ${tuples.mkString("\n\n  ")}
       |}""".stripMargin
  }
}
