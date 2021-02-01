package ai.dragonfly.versionedjson

import ai.dragonfly.versionedjson.native
import ai.dragonfly.versionedjson.Primitive._

import scala.language.postfixOps
import scala.reflect.ClassTag
import scala.collection.{immutable, mutable}
import mutable.ArrayBuffer


/**
 *  object for Utility methods.
 */

object Versioned {
  // implicit conversions:

  implicit def jsValueToJSON_String(v: ujson.Value): String = v.render()

  // for current version readers
  implicit def doubleToVersion(vid: Double)(implicit tag: ClassTag[_ <: Versioned]): Version = Version(tag.toString(), vid, tag)

  // for stale version readers
  implicit def clsToVersion(cls:String)(implicit tag: ClassTag[_ <: Versioned]): Version = Version(
    cls,
    java.lang.Double.parseDouble( tag.toString().split("[$]")(1).replace('_', '.') ),
    tag
  )


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

    def toJSON[T <: Versioned](elements: WritesVersionedJSON[T]*)(implicit versionIndex:VersionIndex): String = {
      if (elements.length < 1) s"""{"$a":[]}""" else {
        val majority: Version = getMajority(elements:_*)
        val sb: mutable.StringBuilder = new StringBuilder(s"""{"$a":[${versionIndex(majority)}""")
        for (e <- elements) sb.append(s", ${
          if (e.version == majority) e.toJSON else e.toVersionedJSON
        }")
        sb.append("]}").toString()
      }
    }

    def fromJSON[V](rawJSON: String)(implicit readers: ReaderCache, tag: ClassTag[V]): Option[Array[V]] = for {
      wrapper <- ujson.read(rawJSON).objOpt
      arr <- wrapper(s"$a").arrOpt
      majorityReaderId <- arr(0).numOpt
    } yield {
      val majorityReader:ReadsJSON[_] = readers(majorityReaderId.toInt)
      val valuesT = arr.tail
      val out: Array[V] = new Array[V](valuesT.length)
      for (i <- valuesT.indices) {
        val vti = valuesT(i).arrOpt
        out(i) = vti match {
          case Some(m: ArrayBuffer[ujson.Value]) =>
            readers[Versioned](m.render()) match {
              case Some(p: Primitive[_]) => p.p.asInstanceOf[V]
              case Some(v: V) => v
              case Some(v0: Versioned) => throw UnknownVersion(v0.version)
              case o: Any => throw TypeNotVersioned(o)
            }
          case _ =>
            majorityReader.fromJSON(valuesT(i)) match {
              case Some(p: Primitive[_]) => p.p.asInstanceOf[V]
              case Some(v: V) => v
              case Some(v0: Versioned) => throw UnknownVersion(v0.version)
              case o: Any => throw TypeNotVersioned(o)
            }
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

    trait Transformer[T] {
      def transform(t: T): WritesVersionedJSON[Versioned]
    }

    class DefaultTransformer[T] extends Transformer[T] {
      override def transform(t: T): WritesVersionedJSON[Versioned] = t match {
        case wvj: WritesVersionedJSON[_] => wvj.asInstanceOf[WritesVersionedJSON[Versioned]]
        case o: Any => throw TypeNotVersioned(o)
      }
    }

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
    def toJSON[K, V](
      map: Map[K, V],
      keyTransformer: Transformer[K] = new DefaultTransformer[K](),
      valueTransformer: Transformer[V] = new DefaultTransformer[V]()
    )(implicit versionIndex:VersionIndex = new VersionIndex(), kTag: ClassTag[WritesVersionedJSON[_]], vTag: ClassTag[WritesVersionedJSON[_]]): String = {
      if (map.size < 1) s"""{"$m":[]}""" else {
        val keyArr: Array[WritesVersionedJSON[Versioned]] = new Array[WritesVersionedJSON[Versioned]](map.size)
        val valArr: Array[WritesVersionedJSON[Versioned]] = new Array[WritesVersionedJSON[Versioned]](map.size)
        var i = 0
        for ((k, v) <- map) {
          keyArr(i) = keyTransformer.transform(k)
          valArr(i) = valueTransformer.transform(v)
          i = i + 1
        }
        s"""{"$m":[${ArrayJSON.toJSON[Versioned](keyArr:_*)},${ArrayJSON.toJSON[Versioned](valArr:_*)}]}"""
      }
    }

    def fromJSON[K, V](rawJSON: String)(implicit readers: ReaderCache, kTag: ClassTag[K], vTag: ClassTag[V]): Option[immutable.Map[K, V]] = for {
      root <- ujson.read(rawJSON).objOpt
      kvArr <- root(s"$m").arrOpt
      kArr <- ArrayJSON.fromJSON[K](kvArr(0))
      vArr <- ArrayJSON.fromJSON[V](kvArr(1))
    } yield kArr zip[V] vArr toMap
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
  implicit val tag: ClassTag[T] = native.ClassTag[T]({
    val tokens = this.getClass.getName.split("\\$")
    if (tokens.length == 1) tokens(0) // current Version
    else tokens(0) + "$" + tokens(1) // stale version
  })
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
    case -1 => VBoolean
    case -2 => VByte
    case -3 => VChar
    case -4 => VDouble
    case -5 => VFloat
    case -6 => VInt
    case -7 => VLong
    case -8 => VShort
    case -9 => VString
  }

  def apply[T <: Versioned](rawJSON: String)(implicit tag: ClassTag[T]): Option[T] = for {
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
        case VBoolean.version => -1
        case VByte.version => -2
        case VChar.version => -3
        case VDouble.version => -4
        case VFloat.version => -5
        case VInt.version => -6
        case VLong.version => -7
        case VShort.version => -8
        case VString.version => -9
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

    def get (version: Version): Option[ReadsJSON[_]] = get(version.cls, version.vid)

    def get (cls: String, vid: Double): Option[ReadsJSON[_]] = {
      for {
        hm <- registry.get(cls)
        reader <- hm.get(vid)
      } yield reader
    }

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