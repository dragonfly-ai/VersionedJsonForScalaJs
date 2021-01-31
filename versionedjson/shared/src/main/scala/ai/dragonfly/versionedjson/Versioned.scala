package ai.dragonfly.versionedjson

import ai.dragonfly.versionedjson.Primitive.Primitive
import ai.dragonfly.versionedjson.native
import ujson.Value

import scala.collection.{immutable, mutable}
import mutable.ArrayBuffer
import scala.reflect.ClassTag

/**
 *  object for Utility methods.
 */

object Versioned {

  // only for stale version readers
  private def parseVid(tag: ClassTag[_]): Double = java.lang.Double.parseDouble(
    tag.toString().split("[$]")(1).replace('_', '.')
  )

  def getTag[T <: Versioned](v: Versioned): ClassTag[T] = native.ClassTag[T]({
    val className: String = v.getClass.getName
    val tokens = className.split("\\$")
    if (tokens.length == 1) tokens(0) // current Version
    else tokens(0) + "$" + tokens(1) // stale version
  })

  implicit def doubleToVersion(vid: Double)(implicit tag: ClassTag[_ <: Versioned]): Version = Version(tag.toString(), vid, tag)

  // only for stale version readers
  implicit def clsToVersion(cls:String)(implicit tag: ClassTag[_ <: Versioned]): Version = Version(cls, parseVid(tag), tag)
  import VersionedJSON.Cargo._

  object ArrayJSON {
    private def getMajority(elements: WritesVersionedJSON[_]*): Version = {
      val hist: mutable.HashMap[Version, Int] = mutable.HashMap[Version, Int]()
      var majorityType: Version = elements(0).version
      var maxCount = 0
      for (e <- elements) {
        val count = hist.getOrElse(e.version, 1)
        hist.put(e.version, count)
        if (count > maxCount) {
          majorityType = e.version
          maxCount = count
        }
      }
      majorityType
    }

    def toJSON(elements: WritesVersionedJSON[_]*)(implicit versionIndex:VersionIndex): String = {
      if (elements.length < 1) s"""{"$a":[]}""" else {
        val majority: Version = getMajority(elements:_*)
        val sb: mutable.StringBuilder = new StringBuilder(s"""{"$a":[${versionIndex(majority)}""")
        for (e <- elements) sb.append(s", ${
          if (e.version == majority) e.toJSON else e.toVersionedJSON
        }")
        sb.append("]}").toString()
      }
    }

    def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[Array[WritesVersionedJSON[_]]] = for {
      wrapper <- ujson.read(rawJSON).objOpt
      arr <- wrapper(s"$a").arrOpt
      majorityReaderId <- arr(0).numOpt
    } yield {
      val majorityReader:ReadsJSON[_] = readers(majorityReaderId.toInt)
      val valuesT = arr.tail
      val out: Array[WritesVersionedJSON[_]] = new Array[WritesVersionedJSON[_]](valuesT.length)
      for (i <- valuesT.indices) {
        out(i) = valuesT(i).arrOpt match {
          case Some(m:ArrayBuffer[ujson.Value]) => readers[WritesVersionedJSON[_]](m.render()).get
          case _ => majorityReader.fromJSON(valuesT(i).render()).get.asInstanceOf[WritesVersionedJSON[_]]
        }
      }
      out
    }
  }

  /**
   * To serialize and deserialize maps:
   */
  object MapJSON {
    import Primitive._
    /**
     * Use this method for maps keyed and valued by Versioned Classes.
     *
     * @param map to serialize
     * @param versionIndex implicitly provided
     * @param kTag implicitly provided
     * @param vTag implicitly provided
     * @tparam K key type
     * @tparam V value type
     * @return serialized JSON String for this map.
     */
    def toJSON[K <: WritesVersionedJSON[_], V <: WritesVersionedJSON[_]](map: Map[K, V])(implicit versionIndex:VersionIndex = new VersionIndex(), kTag: ClassTag[K], vTag: ClassTag[V]): String = {
      if (map.size < 1) s"""{"$m":[]}""" else {
        val keyArr: Array[WritesVersionedJSON[_]] = new Array[WritesVersionedJSON[_]](map.size)
        val valArr: Array[WritesVersionedJSON[_]] = new Array[WritesVersionedJSON[_]](map.size)
        var i = 0
        for ((k: K, v: V) <- map) {
          keyArr(i) = k
          valArr(i) = v
          i = i + 1
        }
        s"""{"$m":[${ArrayJSON.toJSON(keyArr:_*)},${ArrayJSON.toJSON(valArr:_*)}]}"""
      }
    }

    def primativeKeyToJSON[P <: AnyVal, K <: Primitive[P], V <: WritesVersionedJSON[_]](map: Map[P, V], keyTransformer: ReadsPrimitiveJSON[P, K])(implicit versionIndex:VersionIndex = new VersionIndex(), vTag: ClassTag[V]): String = {
      implicit val kTag: ClassTag[K] = keyTransformer.tag
      if (map.size < 1) s"""{"$m":[]}""" else {
        val keyArr: Array[WritesVersionedJSON[_]] = new Array[WritesVersionedJSON[_]](map.size)
        val valArr: Array[WritesVersionedJSON[_]] = new Array[WritesVersionedJSON[_]](map.size)
        var i = 0
        for ((k: P, v: V) <- map) {
          keyArr(i) = keyTransformer(k)
          valArr(i) = v
          i = i + 1
        }
        s"""{"$m":[${ArrayJSON.toJSON(keyArr:_*)},${ArrayJSON.toJSON(valArr:_*)}]}"""
      }
    }

    def primativeValueToJSON[K <: WritesVersionedJSON[_], P <: AnyVal, V <: Primitive[P]](map: Map[K, P], valueTransformer: ReadsPrimitiveJSON[P, V])(implicit versionIndex:VersionIndex = new VersionIndex(), kTag: ClassTag[K]): String = {
      implicit val kTag: ClassTag[V] = valueTransformer.tag
      if (map.size < 1) s"""{"$m":[]}""" else {
        val keyArr: Array[WritesVersionedJSON[_]] = new Array[WritesVersionedJSON[_]](map.size)
        val valArr: Array[WritesVersionedJSON[_]] = new Array[WritesVersionedJSON[_]](map.size)
        var i = 0
        for ((k: K, v: P) <- map) {
          keyArr(i) = k
          valArr(i) = valueTransformer(v)
          i = i + 1
        }
        s"""{"$m":[${ArrayJSON.toJSON(keyArr:_*)},${ArrayJSON.toJSON(valArr:_*)}]}"""
      }
    }


    def primativeKeyAndValueToJSON[PK <: AnyVal, K <: Primitive[PK], PV <: AnyVal, V <: Primitive[PV]](map: Map[PK, PV], keyTransformer: ReadsPrimitiveJSON[PK, K], valueTransformer: ReadsPrimitiveJSON[PV, V])(implicit versionIndex:VersionIndex = new VersionIndex(), kTag: ClassTag[K]): String = {
      if (map.size < 1) s"""{"$m":[]}""" else {
        val keyArr: Array[WritesVersionedJSON[_]] = new Array[WritesVersionedJSON[_]](map.size)
        val valArr: Array[WritesVersionedJSON[_]] = new Array[WritesVersionedJSON[_]](map.size)
        var i = 0
        for ((k: PK, v: PV) <- map) {
          keyArr(i) = keyTransformer(k)
          valArr(i) = valueTransformer(v)
          i = i + 1
        }
        s"""{"$m":[${ArrayJSON.toJSON(keyArr:_*)},${ArrayJSON.toJSON(valArr:_*)}]}"""
      }
    }

    import scala.language.postfixOps

    def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[immutable.Map[WritesVersionedJSON[_], WritesVersionedJSON[_]]] = for {
      root <- ujson.read(rawJSON).objOpt
      kvArr <- root(s"$m").arrOpt
      kArr <- ArrayJSON.fromJSON(kvArr(0).render())
      vArr <- ArrayJSON.fromJSON(kvArr(1).render())
    } yield kArr zip[WritesVersionedJSON[_]] vArr toMap
  }
}

/**
 * Trait for classes and companion objects
 */

trait Versioned extends native.Versioned {
  val version: Version
}

/**
  * traits for classes
  */

trait VersionedClass[T <: Versioned] extends Versioned {
  // Always get Version Info from Companion Object.
  override lazy val version: Version = native.LoadReader[T](this).version
}

/**
 * Current Version only
 */

trait WritesVersionedJSON[T <: Versioned] extends VersionedClass[T] {
  import VersionedJSON.Cargo._
  def toJSON(implicit versionIndex:VersionIndex):String

  def toVersionedJSON(implicit versionIndex: VersionIndex = new VersionIndex()): String = {
    val index = versionIndex(this)
    val root:Boolean = versionIndex.size < 2  // object hierarchy root?
    val versionedJSON = toJSON(versionIndex)
    if (root) {
      s"""{"$v":${versionIndex.toJSON},"$o":[$index,$versionedJSON]}"""
    } else s"""[$index,$versionedJSON]"""
  }
}

/**
 * Past Versions
 * @tparam T type parameter of next most current version.
 */

trait OldVersionOf[T <: Versioned] extends VersionedClass[T] {
  def upgrade: Option[T]
}

/**
  * traits for companion objects
  */

sealed trait ReadsJSON[T <: Versioned] extends Versioned {
  implicit val tag: ClassTag[T] = Versioned.getTag[T](this)
  def fromJSON(rawJSON: String)(implicit readers:ReaderCache): Option[T]
}

/**
  Meant only for the current version of the class.
 */

trait ReadsVersionedJSON[T <: Versioned] extends ReadsJSON[T] {
  val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]]
  def fromVersionedJSON(rawJSON:String)(implicit readers: ReaderCache = null) : Option[T] = {
    if (readers == null) VersionedJSON[T](rawJSON)
    else readers[T](rawJSON)
  }
}

/**
  Only intended for past versions of classes.
 */

trait ReadsStaleJSON[T <: Versioned] extends ReadsJSON[T]

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

  def apply(s: String): string = new string(s)

  trait Primitive[P <: AnyVal] extends WritesVersionedJSON[Primitive[P]] {
    val p: P
    def ujsonValue: ujson.Value
    override def toJSON(implicit versionIndex: VersionIndex): String = ujson.write(ujsonValue)
  }

  trait ReadsPrimitiveJSON[P <: AnyVal, T <: Primitive[P]] extends ReadsVersionedJSON[T] {
    override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()
    def apply(p: AnyVal): T
  }

  object boolean extends ReadsPrimitiveJSON[Boolean, boolean] {
    override val version: Version = Version("boolean", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[boolean] = Some(boolean(java.lang.Boolean.parseBoolean(rawJSON)))

    override def apply(p: AnyVal): boolean = new boolean(p.asInstanceOf[Boolean])
  }
  class boolean(override val p: Boolean) extends Primitive[Boolean] {
    override def ujsonValue: Value = ujson.Bool(p)
  }

  object byte extends ReadsPrimitiveJSON[Byte, byte] {
    override val version: Version = Version("byte", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[byte] = Some(byte(java.lang.Byte.parseByte(rawJSON)))

    override def apply(p: AnyVal): byte = new byte(p.asInstanceOf[Byte])
  }
  class byte(override val p: Byte) extends Primitive[Byte] {
    override def ujsonValue: Value = ujson.Num(p)
  }

  object short extends ReadsPrimitiveJSON[Short, short] {
    override val version: Version = Version("short", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[short] = Some(short(java.lang.Short.parseShort(rawJSON)))

    override def apply(p: AnyVal): short = new short(p.asInstanceOf[Short])
  }
  class short(override val p: Short) extends Primitive[Short] {
    override def ujsonValue: Value = ujson.Num(p.toInt)
  }

  object int extends ReadsPrimitiveJSON[Int, int] {
    override val version: Version = Version("int", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[int] = Some(int(java.lang.Integer.parseInt(rawJSON)))

    override def apply(p: AnyVal): int = new int(p.asInstanceOf[Int])
  }
  class int(override val p: Int) extends Primitive[Int] {
    override def ujsonValue: Value = ujson.Num(p)
  }

  object long extends ReadsPrimitiveJSON[Long, long] {
    override val version: Version = Version("long", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[long] = Some(long(java.lang.Long.parseLong(ujson.read(rawJSON).str)))

    override def apply(p: AnyVal): long = new long(p.asInstanceOf[Long])
  }
  class long(override val p: Long) extends Primitive[Long] {
    override def ujsonValue: Value = ujson.Str(p.toString)
  }

  object float extends ReadsPrimitiveJSON[Float, float] {
    override val version: Version = Version("float", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[float] = Some(float(java.lang.Float.parseFloat(rawJSON)))

    override def apply(p: AnyVal): float = new float(p.asInstanceOf[Float])
  }
  class float(override val p: Float) extends Primitive[Float] {
    override def ujsonValue: Value = ujson.Num(p)
  }

  object double extends ReadsPrimitiveJSON[Double, double] {
    override val version: Version = Version("double", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[double] = Some(double(java.lang.Double.parseDouble(rawJSON)))

    override def apply(p: AnyVal): double = new double(p.asInstanceOf[Double])
  }
  class double(override val p: Double) extends Primitive[Double] {
    override def ujsonValue: Value = ujson.Num(p)
  }

  object char extends ReadsPrimitiveJSON[Char, char] {
    override val version: Version = Version("char", 0.0, tag)
    override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[char] = Some(char(rawJSON.charAt(0)))

    override def apply(p: AnyVal): char = new char(p.asInstanceOf[Char])
  }
  class char(override val p: Char) extends Primitive[Char] {
    override def ujsonValue: Value = ujson.Str(p.toString)
  }

  object string extends ReadsVersionedJSON[string] {
    override val version: Version = Version("java.lang.String", 0.0, tag)
    override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()
    override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[string] = Some(new string(ujson.read(rawJSON).str))
  }
  class string(p: String) extends WritesVersionedJSON[string] {
    def ujsonValue: Value = ujson.Str(p)
    override def toJSON(implicit versionIndex: VersionIndex): String = ujson.write(ujsonValue)
  }
}
/**
 * A class to represent Version Info
 */

object Version {

  def parseVersionString(vString: String): Version = {
    val tokens = vString.split(":")
    Version(tokens(0), java.lang.Double.parseDouble(tokens(1)), null)
  }

}

/**
 * stores version information
 *
 * @param cls fully qualified class name of a versioned class.
 * @param vid version id of a versioned class.
 */

case class Version(cls: String, vid: Double, tag: ClassTag[_ <: Versioned]) {
  def toJSON: String = s""""$cls:$vid""""
  override def toString: String = toJSON
}

object ReaderCache {
  def fromArr(arr:ArrayBuffer[ujson.Value]):ReaderCache = {
    val versionReaders:Array[ReadsJSON[_]] = new Array[ReadsJSON[_]](arr.length)
    for (i <- arr.indices) {
      val version = Version.parseVersionString(arr(i).str)
      versionReaders(i) = VersionedJSON.Readers.get(version).getOrElse({
        throw UnknownVersion(version)
      })
    }
    new ReaderCache(versionReaders)
  }
}

class ReaderCache(readers: Array[ReadsJSON[_]]) {
  import Primitive._
  def apply(index: Int): ReadsJSON[_] = index match {
    case i if i > -1 => readers(i)
    case -1 => boolean
    case -2 => byte
    case -3 => char
    case -4 => double
    case -5 => float
    case -6 => int
    case -7 => long
    case -8 => short
    case -9 => string
  }

  def apply[T <: Versioned](rawJSON: String)(implicit tag: ClassTag[T]): Option[T] = {
    for {
      arr <- ujson.read(rawJSON).arrOpt
      vid <- arr(0).numOpt
    } yield {
      val cargoJSON: String = arr(1).render()
      val reader = readers(vid.toInt).asInstanceOf[ReadsJSON[T]]
      reader.fromJSON(cargoJSON)(this) match {
        case Some(ov:OldVersionOf[_]) => VersionedJSON.upgradeToCurrentVersion[T](ov)
        case Some(wvj: T) => wvj
        case o: Any => throw UnknownJSON(reader.tag, cargoJSON)
      }
    }
  }
}

/**
 * Manages an ephemeral dictionary that compresses version information for nested versioned objects and collections.
 */

class VersionIndex {

  val hist: mutable.HashMap[Version, Int] = mutable.HashMap[Version, Int]()

  def apply(version: Version): Int = {
    import Primitive._
    hist.getOrElse(
      version,
      version match {
        case boolean.version => -1
        case byte.version => -2
        case char.version => -3
        case double.version => -4
        case float.version => -5
        case int.version => -6
        case long.version => -7
        case short.version => -8
        case string.version => -9
        case _ =>
          val index = this.size
          hist.put(version, index)
          index
      }
    )
  }

  def apply(value: Any): Int = {
    value match {
      case primitive: Primitive[_] =>
        apply(primitive.version)
      case versioned: Versioned => apply(versioned.version)
      case _ => throw TypeNotVersioned(value)
    }
  }

  def size: Int = hist.size

  def toJSON: String = {
    if (size < 1) "[]" else {
      val versions:Array[Version] = new Array[Version](size)
      for ((version, index) <- hist) versions(index) = version

      val sb: StringBuilder = new StringBuilder(s"[${versions(0)}")

      for ( i <- 1 until versions.length) sb.append(s",${versions(i)}")

      sb.append("]").toString()
    }
  }
}

/**
  VersionedJSON serialization registry
*/

object VersionedJSON {

  def upgradeToCurrentVersion[T <: Versioned](o: OldVersionOf[_])(implicit tag: ClassTag[T]): T = {
    val ou = o.upgrade
    ou match {
      case Some(ov: OldVersionOf[_]) => upgradeToCurrentVersion[T](ov)
      case Some(nv: T) => nv
      case _ => throw UpgradeFailure(o, ou.get)(tag)
    }
  }

  object Readers {
    private val registry = mutable.HashMap[String, mutable.HashMap[Double, ReadsJSON[_ <: Versioned]]]()

//    import Primitive._
//    this.apply(boolean, byte, char, double, float, int, long, short, string)

    def get (version: Version): Option[ReadsJSON[_]] = get(version.cls, version.vid)

    def get (cls: String, vid: Double): Option[ReadsJSON[_]] = {
      for {
        hm <- registry.get(cls)
        reader <- hm.get(vid)
      } yield reader
    }

    //def get (v: Versioned): Option[ReadsJSON[_]] = this.get(v.version)

    private def put(reader: ReadsJSON[_ <: Versioned]): Option[ReadsJSON[_<: Versioned]] = Some(
      registry.getOrElseUpdate(
        reader.version.cls, mutable.HashMap[Double, ReadsJSON[_ <: Versioned]]()
      ).getOrElseUpdate( reader.version.vid, reader )
    )

    def apply(reader: ReadsJSON[_ <: Versioned]): Option[ReadsJSON[_<: Versioned]] = synchronized {
      reader match {
        case rvj: ReadsVersionedJSON[_] =>
          println(s"[VersionedJSON.Readers] registered ${rvj.version}.")
          rvj.oldVersions.foreach( r => apply(r) )
          put(rvj)
        case rsj: ReadsStaleJSON[_] =>
          println(s"[VersionedJSON.Readers] registered ${rsj.version} to read an old version of ${rsj.tag}")
          put(rsj)
      }
    }

    def apply(cls: String, vid: Double): Option[ReadsJSON[_]] = get(cls, vid)

    override def toString: String = this.registry.toString()
  }

  // Enumeration for JSON metadata labels
  object Cargo extends Enumeration {

    val v:Value = Value("#v")    // Version Registry
    val o:Value = Value("#o")    // Object
    val a:Value = Value("#a")    // Array
    val m:Value = Value("#m")    // Map

    def fromString(pt: String): Cargo.Value = {
      pt match {
        case "#v" => v
        case "#o" => o
        case "#a" => a
        case "#m" => m
      }
    }

    def fromObj(ujsonValue: ujson.Value): Option[Cargo.Value] = Some(
      if (ujsonValue(s"$v") != null) v
      else if (ujsonValue(s"$o") != null) o
      else if (ujsonValue(s"$a") != null) a
      else m //if (ujsonValue(s"$kv") != null) kv
    )
  }

  def apply[T <: Versioned](rawJSON: String)(implicit tag: ClassTag[T]): Option[T] = for {
    wrapper <- ujson.read(rawJSON).objOpt
    versionsArr <- wrapper(s"${Cargo.v}").arrOpt
    cargoArr <- wrapper(s"${Cargo.o}").arrOpt
    rootVersionID <- cargoArr(0).numOpt
    cargo <- cargoArr(1).objOpt
  } yield {
    implicit val versions: ReaderCache = ReaderCache.fromArr(versionsArr)
    val cargoJSON = cargo.render()
    val reader: ReadsJSON[T] = versions(rootVersionID.toInt).asInstanceOf[ReadsJSON[T]]
    reader.fromJSON(cargoJSON) match {
      case Some(currentVersion: T) => currentVersion.asInstanceOf[T]
      case Some(oldVersion: OldVersionOf[_]) => upgradeToCurrentVersion[T](oldVersion)
      case _ => throw UnknownJSON(tag, cargoJSON)
    }
  }
}

case class UnknownJSON(tag: ClassTag[_], rawJSON: String) extends Exception(s"Can't interperet $tag from json: $rawJSON")
case class JSON_ReadFailure(rawJSON: String) extends Exception(s"Could not read JSON: $rawJSON")
case class UnknownReader(v: Versioned) extends Exception(s"No known reader for: $v")
case class UpgradeFailure(oldVersion:Versioned, upgrade: Any)(implicit tag: ClassTag[_]) extends Exception(s"$tag upgrade failure.  Lost the upgrade path after upgrading $oldVersion to $upgrade.")
case class UnknownVersion(version: Version) extends Exception(s"Unknown Versioned Class: $version")
case class TypeNotVersioned(o: Any) extends Exception(s"Type of: $o, ${o.getClass} is not versioned.")
case class MalformedMap(map: ujson.Arr) extends Exception(s"Malformed Map.  Key and Value counts differ: ${map.render()}")