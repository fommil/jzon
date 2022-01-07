object CodeGen {
  def tupleDecoders: String = {
    val decoders = (1 to 22).map { i =>
      val tparams = (1 to i).map(p => s"A$p").mkString(", ")
      val implicits = (1 to i).map(p => s"A$p: Decoder[A$p]").mkString(", ")
      val work = (1 to i).map { p =>
        s"val a$p = A$p.unsafeDecode(traces($p) :: trace, in)"
      }.mkString("\n        Lexer.char(trace, in, ',')\n        ")
      val returns = (1 to i).map(p => s"a$p").mkString(", ")

      s"""implicit def tuple${i}[$tparams](implicit $implicits): Decoder[Tuple${i}[$tparams]] =
       |    new Decoder[Tuple${i}[$tparams]] {
       |      val traces: Array[JsonError] = (0 to $i).map(JsonError.ArrayAccess(_)).toArray
       |      def unsafeDecode(trace: List[JsonError], in: RetractReader): Tuple${i}[$tparams] = {
       |        Lexer.char(trace, in, '[')
       |        $work
       |        Lexer.char(trace, in, ']')
       |        Tuple${i}($returns)
       |      }
       |    }""".stripMargin
    }
    s"""package zio.json
       |
       |import zio.json.internal._
       |
       |private[json] trait GeneratedTupleDecoders { this: Decoder.type =>
       |  ${decoders.mkString("\n\n  ")}
       |}""".stripMargin
  }

  def tupleEncoders: String = {
    val encoders = (1 to 22).map { i =>
      val tparams = (1 to i).map(p => s"A$p").mkString(", ")
      val implicits = (1 to i).map(p => s"A$p: Encoder[A$p]").mkString(", ")
      val work = (1 to i).map { p =>
        s"A$p.unsafeEncode(t._$p, indent, out)"
      }.mkString("\n        if (indent.isEmpty) out.write(\",\") else out.write(\", \")\n        ")

      s"""implicit def tuple${i}[$tparams](implicit $implicits): Encoder[Tuple${i}[$tparams]] =
       |    new Encoder[Tuple${i}[$tparams]] {
       |      def unsafeEncode(t: Tuple${i}[$tparams], indent: Option[Int], out: java.io.Writer): Unit = {
       |        out.write("[")
       |        $work
       |        out.write("]")
       |      }
       |    }""".stripMargin
    }
    s"""package zio.json
       |
       |private[json] trait GeneratedTupleEncoders { this: Encoder.type =>
       |  ${encoders.mkString("\n\n  ")}
       |}""".stripMargin
  }
}
