package ai.dragonfly.versionedjson

import ujson.Value


/**
 * Primitive types.
 * boolean
 * byte
 * short
 * int
 * long
 * float
 * double
 * char
 * class java.lang.String
 */

object Primitive {

  // implicit conversions:
  import scala.language.implicitConversions

  implicit def VBooleanToBoxedBoolean(b: Primitive[Boolean]): java.lang.Boolean = b.p
  implicit def VByteToBoxedByte(b: Primitive[Byte]): java.lang.Byte = b.p
  implicit def VShortToBoxedShort(b: Primitive[Short]): java.lang.Short = b.p
  implicit def VIntToBoxedInteger(i: Primitive[Int]): java.lang.Integer = i.p
  implicit def VLongToBoxedLong(b: Primitive[Long]): java.lang.Long = b.p
  implicit def VFloatToBoxedFloat(b: Primitive[Float]): java.lang.Float = b.p
  implicit def VDoubleToBoxedDouble(b: Primitive[Double]): java.lang.Double = b.p
  implicit def VCharToBoxedChar(b: Primitive[Char]): java.lang.Character = b.p

  implicit def VBooleanToBoolean(b: Primitive[Boolean]): Boolean = b.p
  implicit def VByteToByte(b: Primitive[Byte]): Byte = b.p
  implicit def VShortToShort(b: Primitive[Short]): Short = b.p
  implicit def VIntToInt(i: Primitive[Int]): Int = i.p
  implicit def VLongToLong(b: Primitive[Long]): Long = b.p
  implicit def VFloatToFloat(b: Primitive[Float]): Float = b.p
  implicit def VDoubleToDouble(b: Primitive[Double]): Double = b.p
  implicit def VCharToChar(b: Primitive[Char]): Char = b.p

  implicit def VStringToString(s: VString): String = s.p


  implicit def uvalToOptionBoolean(os: Option[ujson.Value]): Option[Boolean] = os match { case Some(ub:ujson.Bool) => Some(ub.bool) case _ => None }
  implicit def uvalToOptionByte(os: Option[ujson.Value]): Option[Byte] = os match { case Some(ub:ujson.Num) => Some(ub.num.toByte) case _ => None }
  implicit def uvalToOptionShort(os: Option[ujson.Value]): Option[Short] = os match { case Some(us:ujson.Num) => Some(us.num.toShort) case _ => None }
  implicit def uvalToOptionInt(os: Option[ujson.Value]): Option[Int] = os match { case Some(un:ujson.Num) => Some(un.num.toInt) case _ => println(os); None }
  implicit def uvalToOptionLong(os: Option[ujson.Value]): Option[Long] = os match { case Some(us:ujson.Str) => Some(java.lang.Long.parseLong(us.str)) case _ => None }
  implicit def uvalToOptionFloat(os: Option[ujson.Value]): Option[Float] = os match { case Some(un:ujson.Num) => Some(un.num.toFloat) case _ => None }
  implicit def uvalToOptionDouble(os: Option[ujson.Value]): Option[Double] = os match { case Some(un:ujson.Num) => Some(un.num.toDouble) case _ => None }
  implicit def uvalToOptionChar(os: Option[ujson.Value]): Option[Char] = os match { case Some(us:ujson.Str) => Some(us.str.charAt(0)) case _ => None }

  implicit def uvalToOptionString(os: Option[ujson.Value]): Option[String] = os match { case Some(us:ujson.Str) => Option[String](us.str) case _ => None }


  implicit def OptionBooleanToOptionVBoolean(os: Option[Boolean]): Option[VBoolean] = os match { case Some(vs:Boolean) => Some(new VBoolean(vs)) case _ => None }
  implicit def OptionByteToOptionVByte(os: Option[Byte]): Option[VByte] = os match { case Some(vs:Byte) => Some(new VByte(vs)) case _ => None }
  implicit def OptionShortToOptionVShort(os: Option[Short]): Option[VShort] = os match { case Some(vs:Short) => Some(new VShort(vs)) case _ => None }
  implicit def OptionIntToOptionVInt(os: Option[Int]): Option[VInt] = os match { case Some(vs:Int) => Some(new VInt(vs)) case _ => None }
  implicit def OptionLongToOptionVLong(os: Option[Long]): Option[VLong] = os match { case Some(vs:Long) => Some(new VLong(vs)) case _ => None }
  implicit def OptionFloatToOptionVFloat(os: Option[Float]): Option[VFloat] = os match { case Some(vs:Float) => Some(new VFloat(vs)) case _ => None }
  implicit def OptionDoubleToOptionVDouble(os: Option[Double]): Option[VDouble] = os match { case Some(vs:Double) => Some(new VDouble(vs)) case _ => None }
  implicit def OptionCharToOptionVChar(os: Option[Char]): Option[VChar] = os match { case Some(vs:Char) => Some(new VChar(vs)) case _ => None }

  implicit def OptionStringToOptionVString(os: Option[String]): Option[VString] = os match { case Some(vs:String) => Some(new VString(vs)) case _ => None }


  implicit def OptionVBooleanToOptionBoolean(os: Option[Primitive[Boolean]]): Option[Boolean] = os match { case Some(vs:VBoolean) => Option[Boolean](vs.p) case _ => None }
  implicit def OptionVByteToOptionByte(os: Option[Primitive[Byte]]): Option[Byte] = os match { case Some(vs:VByte) => Option[Byte](vs.p) case _ => None }
  implicit def OptionVShortToOptionShort(os: Option[Primitive[Short]]): Option[Short] = os match { case Some(vs:VShort) => Option[Short](vs.p) case _ => None }
  implicit def OptionVIntToOptionInt(os: Option[Primitive[Int]]): Option[Int] = os match { case Some(vs:VInt) => Option[Int](vs.p) case _ => None }
  implicit def OptionVLongToOptionLong(os: Option[Primitive[Long]]): Option[Long] = os match { case Some(vs:VLong) => Option[Long](vs.p) case _ => None }
  implicit def OptionVFloatToOptionFloat(os: Option[Primitive[Float]]): Option[Float] = os match { case Some(vs:VFloat) => Option[Float](vs.p) case _ => None }
  implicit def OptionVDoubleToOptionDouble(os: Option[Primitive[Double]]): Option[Double] = os match { case Some(vs:VDouble) => Option[Double](vs.p) case _ => None }
  implicit def OptionVCharToOptionChar(os: Option[Primitive[Char]]): Option[Char] = os match { case Some(vs:VChar) => Option[Char](vs.p) case _ => None }

  implicit def OptionVStringToOptionString(os: Option[VString]): Option[String] = os match { case Some(vs:VString) => Option[String](vs.p) case _ => None }

  def apply(s: String): VString = new VString(s)

  trait Primitive[P <: AnyVal] extends WritesVersionedJSON[Primitive[P]] {
    val p: P
    def ujsonValue: ujson.Value
    override def toJSON(implicit versionIndex: VersionIndex): String = ujson.write(ujsonValue)
  }

  trait ReadsPrimitiveJSON[P <: AnyVal] extends ReadsVersionedJSON[Primitive[P]] with Versioned.MapJSON.Transformer[P] {
    override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()
    def apply(p: AnyVal): Primitive[P]
    def apply(rawJSON: String)(implicit readerCache: ReaderCache): Option[Primitive[P]] = this.fromJSON(rawJSON)
    def reader: ReadsPrimitiveJSON[P] = this
    override def transform(t: P): WritesVersionedJSON[Versioned] = apply(t).asInstanceOf[WritesVersionedJSON[Versioned]]
  }

  object VBoolean extends ReadsPrimitiveJSON[Boolean] {
    override val version: Version = Version("boolean", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readerCache: ReaderCache): Option[VBoolean] = Some(VBoolean(java.lang.Boolean.parseBoolean(rawJSON)))
    override def apply(p: AnyVal): VBoolean = new VBoolean(p.asInstanceOf[Boolean])
  }
  class VBoolean(override val p: Boolean) extends Primitive[Boolean] {
    override def ujsonValue: Value = ujson.Bool(p)
  }

  object VByte extends ReadsPrimitiveJSON[Byte] {
    override val version: Version = Version("byte", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readerCache: ReaderCache): Option[VByte] = Some(VByte(java.lang.Byte.parseByte(rawJSON)))

    override def apply(p: AnyVal): VByte = new VByte(p.asInstanceOf[Byte])
  }
  class VByte(override val p: Byte) extends Primitive[Byte] {
    override def ujsonValue: Value = ujson.Num(p)
  }

  object VShort extends ReadsPrimitiveJSON[Short] {
    override val version: Version = Version("short", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readerCache: ReaderCache): Option[VShort] = Some(VShort(java.lang.Short.parseShort(rawJSON)))

    override def apply(p: AnyVal): VShort = new VShort(p.asInstanceOf[Short])
  }
  class VShort(override val p: Short) extends Primitive[Short] {
    override def ujsonValue: Value = ujson.Num(p.toInt)
  }

  object VInt extends ReadsPrimitiveJSON[Int] {
    override val version: Version = Version("int", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readerCache: ReaderCache): Option[VInt] = Some(VInt(java.lang.Integer.parseInt(rawJSON)))

    override def apply(p: AnyVal): VInt = new VInt(p.asInstanceOf[Int])
  }
  class VInt(override val p: Int) extends Primitive[Int] {
    override def ujsonValue: Value = ujson.Num(p)
  }

  object VLong extends ReadsPrimitiveJSON[Long] {
    override val version: Version = Version("long", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readerCache: ReaderCache): Option[VLong] = Some(VLong(java.lang.Long.parseLong(ujson.read(rawJSON).str)))

    override def apply(p: AnyVal): VLong = new VLong(p.asInstanceOf[Long])
  }
  class VLong(override val p: Long) extends Primitive[Long] {
    override def ujsonValue: Value = ujson.Str(p.toString)
  }

  object VFloat extends ReadsPrimitiveJSON[Float] {
    override val version: Version = Version("float", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readerCache: ReaderCache): Option[VFloat] = Some(VFloat(java.lang.Float.parseFloat(rawJSON)))

    override def apply(p: AnyVal): VFloat = new VFloat(p.asInstanceOf[Float])
  }
  class VFloat(override val p: Float) extends Primitive[Float] {
    override def ujsonValue: Value = ujson.Num(p)
  }

  object VDouble extends ReadsPrimitiveJSON[Double] {
    override val version: Version = Version("double", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readerCache: ReaderCache): Option[VDouble] = Some(VDouble(java.lang.Double.parseDouble(rawJSON)))

    override def apply(p: AnyVal): VDouble = new VDouble(p.asInstanceOf[Double])
  }
  class VDouble(override val p: Double) extends Primitive[Double] {
    override def ujsonValue: Value = ujson.Num(p)
  }

  object VChar extends ReadsPrimitiveJSON[Char] {
    override val version: Version = Version("char", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readerCache: ReaderCache): Option[VChar] = Some(VChar(ujson.read(rawJSON).str.charAt(0)))

    override def apply(p: AnyVal): VChar = new VChar(p.asInstanceOf[Char])
  }
  class VChar(override val p: Char) extends Primitive[Char] {
    override def ujsonValue: Value = ujson.Str(p.toString)
  }

  object VString extends ReadsVersionedJSON[VString] {
    override val version: Version = Version("java.lang.String", 0.0, tag)
    override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()
    override def fromJSON(rawJSON: String)(implicit readerCache: ReaderCache): Option[VString] = Some(new VString(ujson.read(rawJSON).str))
    def apply(rawJSON: String)(implicit readerCache: ReaderCache): Option[String] = this.fromJSON(rawJSON)
    def apply(os:Option[String]):Option[VString] = os match {
      case Some(s) => Some(new VString(s))
      case _ => None
    }
  }
  class VString(val p: String) extends WritesVersionedJSON[VString] {
    def ujsonValue: Value = ujson.Str(p)
    override def toJSON(implicit versionIndex: VersionIndex): String = ujson.write(ujsonValue)
  }
}

