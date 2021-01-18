package ai.dragonfly.versionedjson

import ai.dragonfly.versionedjson.native

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
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

  implicit def doubleToVersion(vid: Double)(implicit tag: ClassTag[_]): Version = Version(tag.toString(), vid)

  // only for stale version readers
  implicit def clsToVersion(cls:String)(implicit tag: ClassTag[_]): Version = Version(cls, parseVid(tag))

  import VersionedJSON.Cargo._

  object UniformArray {
    def toJSON(elements: WritesVersionedJSON[_ <: Versioned]*)(implicit versionIndex:VersionIndex): String = {
      if (elements.length < 1) s"""{"$au":[]}""" else {
        val sb: mutable.StringBuilder = new StringBuilder(s"""{"$au":[${versionIndex(elements.head.version)}""")
        for (e <- elements) sb.append(s", ${e.toJSON}")
        sb.append("]}").toString()
      }
    }

    def fromJSON[T <: Versioned](rawJSON: String)(implicit readers:Array[ReadsJSON[_]]): Option[Array[T]] = for {
      root <- ujson.read(rawJSON).objOpt
      uniformArray <- root(s"$au").arrOpt
      readerID <- uniformArray(0).numOpt
    } yield {
      val reader:ReadsJSON[_] = readers(readerID.toInt)
      implicit val tag: ClassTag[T] = reader.tag.asInstanceOf[ClassTag[T]]
      val valuesT = uniformArray.tail
      val out: Array[T] = new Array[T](valuesT.length)
      for (i <- valuesT.indices) out(i) = reader.fromJSON(valuesT(i).render()).get.asInstanceOf[T]
      out
    }
  }

  object DiverseArray {
    import VersionedJSON.Cargo._
    def toJSON(elements: WritesVersionedJSON[_ <: Versioned]*)(implicit versionIndex:VersionIndex): String = {
      if (elements.length < 1) s"""{"$ad":[]}""" else { // diverse Array
        val sb: mutable.StringBuilder = new StringBuilder(s"""{"$ad":[""")
        sb.append(Element.toJSON(elements.head))
        for (e <- elements.tail) sb.append(s", ${Element.toJSON(e)}")
        sb.append("]}").toString()
      }
    }

    def fromJSON(rawJSON: String)(implicit readers:Array[ReadsJSON[_]]): Option[Array[VersionedClass[_]]] = for {
      root <- ujson.read(rawJSON).objOpt
      diverseArray <- root(s"$ad").arrOpt
    } yield {
      val out: Array[VersionedClass[_]] = new Array[VersionedClass[_]](diverseArray.length)
      for (i <- diverseArray.indices) out(i) = Element.fromJSON[VersionedClass[_]](diverseArray(i).render()).get
      out
    }
  }


  object Element {
    def toJSON(element: WritesVersionedJSON[_ <: Versioned])(implicit versionIndex:VersionIndex):String = s"""${element.toVersionedJSON}""" //s"""[${versionIndex(element.version)}, ${element.toVersionedJSON}]"""
    def fromJSON[T <: Versioned](rawJSON: String)(implicit readers:Array[ReadsJSON[_]], tag: ClassTag[T]): Option[T] = {
      for {
        arr <- ujson.read(rawJSON).arrOpt
        vid <- arr(0).numOpt
      } yield {
        val cargoJSON: String = arr(1).render()
        val reader = readers(vid.toInt).asInstanceOf[ReadsJSON[T]]
        reader.fromJSON(cargoJSON) match {
          case Some(ov:OldVersionOf[_]) => VersionedJSON.upgradeToCurrentVersion[T](ov)
          case Some(wvj: T) => wvj
          case o: Any => throw UnknownJSON(reader.tag, cargoJSON)
        }
      }
    }
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
    val index = versionIndex(this.version)
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
  def fromJSON(rawJSON: String)(implicit readers:Array[ReadsJSON[_]]): Option[T]
}

/**
  Meant only for the current version of the class.
 */

trait ReadsVersionedJSON[T <: Versioned] extends ReadsJSON[T] {
  val oldVersions: Array[ReadsStaleJSON[_]]
  def fromVersionedJSON(rawJSON:String)(implicit readers:Array[ReadsJSON[_]] = null) : Option[T] = {
    if (readers == null) VersionedJSON[T](rawJSON)
    else Versioned.Element.fromJSON[T](rawJSON)
  }
}

/**
  Only intended for past versions of classes.
 */

trait ReadsStaleJSON[T <: Versioned] extends ReadsJSON[T]

object Version {

  def parseVersionString(vString: String): Version = {
    val tokens = vString.split(":")
    Version(tokens(0), java.lang.Double.parseDouble(tokens(1)))
  }

  def lookup(cls: String, vid: Double): Version = {
    VersionedJSON.Readers.get(cls, vid) match {
      case Some(_) => new Version(cls, vid)
      case _ => throw UnknownReader(Version(cls, vid))
    }
  }
}

/**
 * stores version information
 *
 * @param cls fully qualified class name of a versioned class.
 * @param vid version id of a versioned class.
 */

case class Version(cls: String, vid: Double) {
  def toJSON: String = s""""$cls:$vid""""
  override def toString(): String = toJSON
}

object VersionIndex {
  def fromArr(arr:ArrayBuffer[ujson.Value]):Array[ReadsJSON[_]] = {
    val versionReaders:Array[ReadsJSON[_]] = new Array[ReadsJSON[_]](arr.length)
    for (i <- arr.indices) {
      val version = Version.parseVersionString(arr(i).str)
      versionReaders(i) = VersionedJSON.Readers.get(version).getOrElse({
        throw UnknownVersionedClass(version)
      })
    }
    versionReaders
  }
}

/**
 * Manages an ephemeral dictionary that compresses version information for nested versioned objects and collections.
 */

class VersionIndex {

  val hist: mutable.HashMap[Version, Int] = mutable.HashMap[Version, Int]()

  def apply(version: Version): Int = {
    hist.get(version) match {
      case Some(index: Int) => index
      case None =>
        val index = hist.size
        hist.put(version, index)
        index
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
      case _ => throw UpgradeFailure(o, ou.get)
    }
  }

  object Readers {
    private val registry = mutable.HashMap[String, mutable.HashMap[Double, ReadsJSON[_]]]()

    private def put (reader: ReadsJSON[_]): Unit = {
      val hm: mutable.HashMap[Double, ReadsJSON[_]] = registry.get(reader.version.cls) match {
        case Some(hm) => hm
        case None =>
          val temp = new mutable.HashMap[Double, ReadsJSON[_]]()
          registry.put(reader.version.cls, temp)
          temp
      }
      hm.put(reader.version.vid, reader)
    }

    def get (version: Version): Option[ReadsJSON[_]] = get(version.cls, version.vid)

    def get (cls: String, vid: Double): Option[ReadsJSON[_]] = {
      for {
        hm <- registry.get(cls)
        reader <- hm.get(vid)
      } yield reader
    }

    def apply(readers: ReadsVersionedJSON[_]*):Readers.type = synchronized {
      for (r <- readers) {
        println(s"[VersionedJSON.Readers] registered ${r.version}.")
        put(r)
        for (ov <- r.oldVersions) {
          println(s"[VersionedJSON.Readers] registered ${ov.version} to read an old version of ${r.tag}")
          put(ov)
        }
      }
      this
    }

    def apply(cls: String, vid: Double): Option[ReadsJSON[_]] = get(cls, vid)

    override def toString: String = this.registry.toString()
  }

  // Enumeration for JSON metadata labels
  object Cargo extends Enumeration {

    val v:Value = Value("#v")    // version array
    val o:Value = Value("#o")    // Object
    val au:Value = Value("#au")  // uniform Array
    val ad:Value = Value("#ad")  // diverse Array
    val kv:Value = Value("#kv")  // Map

    def fromString(pt: String): Cargo.Value = {
      pt match {
        case "#v" => v
        case "#obj" => o
        case "#au" => au
        case "#ad" => ad
        case "#kv" => kv
      }
    }

    def fromObj(ujsonValue: ujson.Value): Option[Cargo.Value] = Some(
      if (ujsonValue(s"$v") != null) v
      else if (ujsonValue(s"$o") != null) o
      else if (ujsonValue(s"$au") != null) au
      else if (ujsonValue(s"$ad") != null) ad
      else kv //if (ujsonValue(s"$kv") != null) kv
    )
  }

  def apply[T <: Versioned](rawJSON: String)(implicit tag: ClassTag[T]): Option[T] = for {
    wrapper <- ujson.read(rawJSON).objOpt
    versionsArr <- wrapper(s"${Cargo.v}").arrOpt
    cargoArr <- wrapper(s"${Cargo.o}").arrOpt
    rootVersionID <- cargoArr(0).numOpt
    cargo <- cargoArr(1).objOpt
  } yield {
    implicit val versions: Array[ReadsJSON[_]] = VersionIndex.fromArr(versionsArr)
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
case class UnknownReader(version: Version) extends Exception(s"No known reader for: $version")
case class UpgradeFailure(oldVersion:Versioned, upgrade: Any)(implicit tag: ClassTag[_]) extends Exception(s"$tag upgrade failure.  Lost the upgrade path after upgrading $oldVersion to $upgrade.")
case class UnknownVersionedClass(version: Version) extends Exception(s"Unknown Versioned Class: $version")