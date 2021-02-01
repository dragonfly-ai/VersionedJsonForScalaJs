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
  implicit def VBooleanToBoolean(b: Primitive[Boolean]): java.lang.Boolean = b.p
  implicit def VByteToByte(b: Primitive[Byte]): java.lang.Byte = b.p
  implicit def VShortToShort(b: Primitive[Short]): java.lang.Short = b.p
  implicit def VIntToInteger(i: Primitive[Int]): java.lang.Integer = i.p
  implicit def VLongToLong(b: Primitive[Long]): java.lang.Long = b.p
  implicit def VFloatToFloat(b: Primitive[Float]): java.lang.Float = b.p
  implicit def VDoubleToDouble(b: Primitive[Double]): java.lang.Double = b.p
  implicit def VCharToChar(b: Primitive[Char]): java.lang.Character = b.p

  implicit def booleanToBoolean(b: Primitive[Boolean]): Boolean = b.p
  implicit def byteToByte(b: Primitive[Byte]): Byte = b.p
  implicit def shortToShort(b: Primitive[Short]): Short = b.p
  implicit def intToInteger(i: Primitive[Int]): Int = i.p
  implicit def longToLong(b: Primitive[Long]): Long = b.p
  implicit def floatToFloat(b: Primitive[Float]): Float = b.p
  implicit def doubleToDouble(b: Primitive[Double]): Double = b.p
  implicit def charToChar(b: Primitive[Char]): Char = b.p

  implicit def stringToString(s: VString): String = s.p

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
    override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[VBoolean] = Some(VBoolean(java.lang.Boolean.parseBoolean(rawJSON)))
    override def apply(p: AnyVal): VBoolean = new VBoolean(p.asInstanceOf[Boolean])
  }
  class VBoolean(override val p: Boolean) extends Primitive[Boolean] {
    override def ujsonValue: Value = ujson.Bool(p)
  }

  object VByte extends ReadsPrimitiveJSON[Byte] {
    override val version: Version = Version("byte", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[VByte] = Some(VByte(java.lang.Byte.parseByte(rawJSON)))

    override def apply(p: AnyVal): VByte = new VByte(p.asInstanceOf[Byte])
  }
  class VByte(override val p: Byte) extends Primitive[Byte] {
    override def ujsonValue: Value = ujson.Num(p)
  }

  object VShort extends ReadsPrimitiveJSON[Short] {
    override val version: Version = Version("short", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[VShort] = Some(VShort(java.lang.Short.parseShort(rawJSON)))

    override def apply(p: AnyVal): VShort = new VShort(p.asInstanceOf[Short])
  }
  class VShort(override val p: Short) extends Primitive[Short] {
    override def ujsonValue: Value = ujson.Num(p.toInt)
  }

  object VInt extends ReadsPrimitiveJSON[Int] {
    override val version: Version = Version("int", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[VInt] = Some(VInt(java.lang.Integer.parseInt(rawJSON)))

    override def apply(p: AnyVal): VInt = new VInt(p.asInstanceOf[Int])
  }
  class VInt(override val p: Int) extends Primitive[Int] {
    override def ujsonValue: Value = ujson.Num(p)
  }

  object VLong extends ReadsPrimitiveJSON[Long] {
    override val version: Version = Version("long", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[VLong] = Some(VLong(java.lang.Long.parseLong(ujson.read(rawJSON).str)))

    override def apply(p: AnyVal): VLong = new VLong(p.asInstanceOf[Long])
  }
  class VLong(override val p: Long) extends Primitive[Long] {
    override def ujsonValue: Value = ujson.Str(p.toString)
  }

  object VFloat extends ReadsPrimitiveJSON[Float] {
    override val version: Version = Version("float", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[VFloat] = Some(VFloat(java.lang.Float.parseFloat(rawJSON)))

    override def apply(p: AnyVal): VFloat = new VFloat(p.asInstanceOf[Float])
  }
  class VFloat(override val p: Float) extends Primitive[Float] {
    override def ujsonValue: Value = ujson.Num(p)
  }

  object VDouble extends ReadsPrimitiveJSON[Double] {
    override val version: Version = Version("double", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[VDouble] = Some(VDouble(java.lang.Double.parseDouble(rawJSON)))

    override def apply(p: AnyVal): VDouble = new VDouble(p.asInstanceOf[Double])
  }
  class VDouble(override val p: Double) extends Primitive[Double] {
    override def ujsonValue: Value = ujson.Num(p)
  }

  object VChar extends ReadsPrimitiveJSON[Char] {
    override val version: Version = Version("char", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[VChar] = Some(VChar(ujson.read(rawJSON).str.charAt(0)))

    override def apply(p: AnyVal): VChar = new VChar(p.asInstanceOf[Char])
  }
  class VChar(override val p: Char) extends Primitive[Char] {
    override def ujsonValue: Value = ujson.Str(p.toString)
  }

  object VString extends ReadsVersionedJSON[VString] {
    override val version: Version = Version("java.lang.String", 0.0, tag)
    override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()
    override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[VString] = Some(new VString(ujson.read(rawJSON).str))
    def apply(rawJSON: String)(implicit readerCache: ReaderCache): Option[VString] = this.fromJSON(rawJSON)
  }
  class VString(val p: String) extends WritesVersionedJSON[VString] {
    def ujsonValue: Value = ujson.Str(p)
    override def toJSON(implicit versionIndex: VersionIndex): String = ujson.write(ujsonValue)
  }
}

