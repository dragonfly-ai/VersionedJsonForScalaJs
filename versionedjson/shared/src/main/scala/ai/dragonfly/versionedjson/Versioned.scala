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
  def fromJSON(rawJSON: String)(implicit versions:Array[Version]): Option[T]
}

/**
  Meant only for the current version of the class.
 */

trait ReadsVersionedJSON[T <: Versioned] extends ReadsJSON[T] {

  val oldVersions: Array[ReadsStaleJSON[_]]

  def fromVersionedJSON(rawJSON:String): Option[WritesVersionedJSON[_]] = VersionedJSON(rawJSON)
  /*
  {
    VersionedJSON.Readers(this)  // only register readers the first time this method is called.
    (rawJSON:String) => VersionedJSON(rawJSON)
  }
   */
  //private def cls: String = this.getClass.getName.split("[$]")(0)
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
//
//  def fromJSON(rawJSON: String): Option[Version] = {
//    println(rawJSON)
//    ujson.read(rawJSON).strOpt match {
//      case Some(s:String) => Some(parseVersionString(s))
//      case _ => None
//    }
//  }

  def lookup(cls: String, vid: Double): Version = {
    VersionedJSON.Readers.get(cls, vid) match {
      case Some(_) => new Version(cls, vid)
      case _ => throw UnknownReader(s"No VersionedJSON reader found for: Version($cls, $vid).  If it exists, try registering it with VersionedJSON.Readers.")
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
  def fromArr(arr:ArrayBuffer[ujson.Value]):Array[Version] = {
    val versions:Array[Version] = new Array[Version](arr.length)
    for (i <- arr.indices) versions(i) = Version.parseVersionString(arr(i).str)
    versions
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

// This is only for arrays of versioned Objects.
object VersionedArray {
  import VersionedJSON.Cargo._
  def toJSON(elements: WritesVersionedJSON[_ <: Versioned]*)(implicit versionIndex:VersionIndex): String = {
    if (elements.length < 1) s"""{"$au":[]}""" else {
      val versions: mutable.HashSet[Version] = mutable.HashSet[Version]()
      for (e <- elements) versions.add(e.version)
      if (versions.size == 1) { // uniform Array
        val sb: mutable.StringBuilder = new StringBuilder(s"""{"$au":[${versionIndex(versions.head)},""")
        sb.append(elements.head.toJSON)
        for (e <- elements.tail) sb.append(s", ${e.toJSON}")
        sb.append("]}").toString()
      } else { // diverse Array
        val sb: mutable.StringBuilder = new StringBuilder(s"""{"$ad":[""")
        sb.append(VersionedElement.toJSON(elements.head))
        for (e <- elements.tail) sb.append(s", ${VersionedElement.toJSON(e)}")
        sb.append("]}").toString()
      }
    }
  }

  def fromJSON(rawJSON: String)(implicit versions:Array[Version]): Option[Array[_ <: VersionedClass[_]]] = {
    val root = ujson.read(rawJSON).obj
    if (root.contains(s"$ad")) {
      val diverseArray: ArrayBuffer[ujson.Value] = root(s"$ad").arr
      val out: Array[VersionedClass[_]] = new Array[VersionedClass[_]](diverseArray.length)
      for (i <- diverseArray.indices) {
        //println(s"VersionedArray.fromJSON($rawJSON)\n\t${diverseArray(i).render()}")
        out(i) = VersionedElement.fromJSON(diverseArray(i).render()).get.asInstanceOf[VersionedClass[_]]
      }
      Some(out)
    } else {
      val uniformArray:ArrayBuffer[ujson.Value] = root(s"$au").arr
      val version = versions(uniformArray(0).num.toInt)
      VersionedJSON.Readers.get(version) match {
        case Some(reader) =>
          val out: Array[VersionedClass[_]] = new Array[VersionedClass[_]](uniformArray.length - 1)
          for (i <- 1 until uniformArray.length) {
            out(i-1) = reader.fromJSON(uniformArray(i).render()).get.asInstanceOf[VersionedClass[_]]
          }
          Some(out)
        case _ => throw new UnknownReader(s"Can't find reader for VersionedClass: $Version")
      }
    }
  }
}

object VersionedElement {
  def toJSON(element: WritesVersionedJSON[_ <: Versioned])(implicit versionIndex:VersionIndex):String = s"""${element.toVersionedJSON}""" //s"""[${versionIndex(element.version)}, ${element.toVersionedJSON}]"""
  def fromJSON(rawJSON: String)(implicit versions:Array[Version]): Option[WritesVersionedJSON[_]] = {
    //println(s"VersionedElement.fromJSON($rawJSON)")
    for {
      arr <- ujson.read(rawJSON).arrOpt
      vid <- arr(0).numOpt
      reader <- VersionedJSON.Readers.get(versions(vid.toInt))
      payload <- reader.fromJSON(arr(1).render())
    } yield {
       payload match {
        case ov:OldVersionOf[_] => VersionedJSON.upgrade(ov).get
        case wvj: WritesVersionedJSON[_] => wvj
        case o: Any => throw UnknownVersionedClass(s"${reader.tag} read: $o which matches no known Versioned Class. ")
      }
    }
  }

//  {
//    val obj: ujson.Obj = ujson.read(rawJSON).obj
//    val version = versions(obj("i").num.toInt)
//    VersionedJSON.Readers.get(version) match {
//      case Some(reader) => reader.fromJSON(obj("e").render()).get.asInstanceOf[VersionedClass[_]]
//      case _ => throw new UnknownReader(s"Can't find reader for VersionedClass: $Version")
//    }
//  }
}


/**
  VersionedJSON serialization registry
*/

object VersionedJSON {

  def upgrade(o: OldVersionOf[_]): Option[WritesVersionedJSON[_]] = {
    val ou = o.upgrade
    ou match {
      case Some(ov: OldVersionOf[_]) => upgrade(ov)
      case Some(nv: WritesVersionedJSON[_]) => Some(nv)
      case _ => throw UpgradeFailure(s"Upgrade failure.  Lost the upgrade path after upgrading $o to $ou.")
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

  /** Wrappers: */
//  def wrap(v: WritesVersionedJson): String = wrap(v.vid, v.cls, v.toJSON, Cargo.obj)
//  def wrap(arr: ArrayOf[WritesVersionedJson]): String = wrap( arr.vid, arr.cls, arr.toJSON, Cargo.ar )

  def unwrap(rawJSON: String): Option[Versioned] = { // only for root level deserializations
    val wrapper = ujson.read(rawJSON)
    implicit val versions: Array[Version] = VersionIndex.fromArr(wrapper(s"${Cargo.v}").arr)
    val cargo: ArrayBuffer[ujson.Value] = wrapper(s"${Cargo.o}").arr
    val rootVersion = versions(cargo(0).num.toInt)
    Readers.get(rootVersion) match {
      case Some(rdr: ReadsJSON[Versioned]) => rdr.fromJSON(cargo(1).render())
      case _ => throw UnknownReader(s"VersionedJSON.Reader found Unregistered Version: $rootVersion")
    }
  }

  def apply[T <: Versioned](rawJSON: String)(implicit tag: ClassTag[T]): Option[WritesVersionedJSON[_]] = {
    unwrap(rawJSON) match {
      case Some(currentVersion: WritesVersionedJSON[T]) => Some(currentVersion)
      case Some(oldVersion: OldVersionOf[_]) => upgrade(oldVersion)
      //case Some(arrayOf:ArrayJSON[T]) => Some(arrayOf.asInstanceOf[T])
      case o: Any => throw UnknownJSON(s"Can't deserialize unknown version of $tag: $o")
    }
  }
}

case class UnknownJSON(msg: String) extends Exception(msg)
case class JSON_ReadFailure(msg: String) extends Exception(msg)
case class UnknownReader(msg: String) extends Exception(msg)
case class UpgradeFailure(msg: String) extends Exception(msg)
case class UnknownVersionedClass(msg: String) extends Exception(msg)