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
  override val version: Version = native.InferVersionFromReader[T](this)
}

/**
 * Current Version only
 */
trait WritesVersionedJSON[T <: Versioned] extends VersionedClass[T] {
  def toJSON: String
  def toVersionedJSON: String = s"""{"#vid":${version.vid},"#cls":"${version.cls}","#obj":${toJSON}}"""
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
  def fromJSON(rawJSON: String): Option[T]

  implicit val tag: ClassTag[T] = Versioned.getTag[T](this)
}

/**
  Meant only for the current version of the class.
 */

trait ReadsVersionedJSON[T <: Versioned] extends ReadsJSON[T] {

  val oldVersions: Array[ReadsStaleJSON[_]]

  lazy val fromVersionedJSON: String => Option[T] = {
    VersionedJSON.Readers(this)  // only register readers the first time this method is called.
    (rawJSON:String) => VersionedJSON[T](rawJSON)
  }
  //private def cls: String = this.getClass.getName.split("[$]")(0)
}

/**
  Only intended for past versions of classes.
 */

trait ReadsStaleJSON[T <: Versioned] extends ReadsJSON[T]

object Version {
  def fromJSON(rawJSON: String): Option[Version] = fromObj(ujson.read(rawJSON).obj)

  def fromObj(jsonObj: ujson.Obj): Option[Version] = for {
    cls <- jsonObj("c").strOpt
    vid <- jsonObj("v").numOpt
  } yield Version(cls, vid)
}

/**
 * stores version information
 *
 * @param cls fully qualified class name of a versioned class.
 * @param vid version id of a versioned class.
 */

case class Version(cls: String, vid: Double) {
  def toJSON: String = s"""{"c":"$cls", "v":$vid}"""
}

/**
 *
 */

class VersionIndexMap {

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
}

// This is only for arrays of versioned Objects.
object VersionedArray {

  def toJSON(elements: WritesVersionedJSON[_ <: Versioned]*): String = {
    val vim: VersionIndexMap = new VersionIndexMap()
    val vArr: Array[VersionedArrayElement] = new Array[VersionedArrayElement](elements.length)
    var i = 0
    for (e <- elements) {
      vArr(i) = VersionedArrayElement(vim(e.version), e)
      i = i + 1
    }
    val versionData:Array[Version] = new Array[Version](vim.hist.size)
    for ((version, index) <- vim.hist) versionData(index) = version
    VersionedArray(versionData, vArr).toJSON
  }

  def fromObj(jsonObj: ujson.Obj): Option[Array[Versioned]] = {
    for {
      arr <- jsonObj(VersionedJSON.Cargo.ar.toString).objOpt
      versionObjs: ArrayBuffer[ujson.Value] <- arr("vs").arrOpt
      valueObjs: ArrayBuffer[ujson.Value] <- arr("es").arrOpt
    } yield {
      implicit val versions: Array[Version] = new Array[Version](versionObjs.length)
      for (i <- versionObjs.indices) versions(i) = Version.fromJSON(versionObjs(i).render()).orNull
      val vs: Array[Versioned] = new Array[Versioned](valueObjs.length)
      for (i <- valueObjs.indices) vs(i) = VersionedArrayElement.fromObj(valueObjs(i).obj).orNull
      return Some(vs)
    }
  }
}

object VersionedArrayElement {
  def fromObj(jsonObj: ujson.Obj)(implicit versions: Array[Version]): Option[Versioned] = for {
    i <- jsonObj("i").numOpt
    elementObj <- jsonObj("e").objOpt
    reader <- VersionedJSON.Readers.get(versions(i.toInt))
    element <- reader.fromJSON(elementObj.render())
  } yield element.asInstanceOf[Versioned]
}

case class VersionedArrayElement(index: Int, element: WritesVersionedJSON[_ <: Versioned]) {
  def toJSON:String = s"""{"i":$index, "e":${element.toJSON}}"""
}

case class VersionedArray (versions: Array[Version], elements: Array[VersionedArrayElement]) {
  def toJSON: String = {
    s"""{"${VersionedJSON.Cargo.ar}":{${
      if (elements.length < 1) """"vs":[],"es":[]"""
      else {
        val sb = new StringBuilder(s""""vs":[${versions(0).toJSON}""")
        for(i <- 1 until versions.length) sb.append(",").append(versions(i).toJSON)
        sb.append("""],"es":[""").append( elements(0).toJSON )
        for (i <- 1 until elements.length) sb.append(",").append(elements(i).toJSON)
        sb.append("]").toString()
      }
    }}}"""
  }
}

/**
  VersionedJSON serialization registry
*/

object VersionedJSON {

  def upgrade[T <: Versioned](o: OldVersionOf[_])(implicit tag: ClassTag[T]): Option[T] = {
    o.upgrade match {
      case Some(ov: OldVersionOf[_]) => upgrade(ov)
      case Some(nv: T) => Some(nv)
      case uhoh: Any => throw UpgradeFailure(s"$tag upgrade failure.  Lost the upgrade path after upgrading $o to $uhoh.")
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
        println(s"Registered Reader ${r.version.cls}")
        put(r)
        for (ov <- r.oldVersions) {
          println(s"Registered Old Version Reader ${ov.getClass.getName}")
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

    val obj:Value = Value("#obj")
    val ar:Value = Value("#ar")
    val kv:Value = Value("#kv")

    def fromString(pt: String): Cargo.Value = {
      pt match {
        case "#obj" => obj
        case "#ar" => ar
        case "#kv" => kv
      }
    }

    def fromObj(ujsonValue: ujson.Value): Option[Cargo.Value] = Some(
      if (ujsonValue(s"$obj") != null) obj
      else if (ujsonValue(s"$ar") != null) ar
      else kv //if (ujsonValue(s"$kv") != null) kv
    )
  }

  /** Wrappers: */
//  def wrap(v: WritesVersionedJson): String = wrap(v.vid, v.cls, v.toJSON, Cargo.obj)
//  def wrap(arr: ArrayOf[WritesVersionedJson]): String = wrap( arr.vid, arr.cls, arr.toJSON, Cargo.ar )

  def unwrap(rawJSON: String): Option[Versioned] = {
    val wrapper = ujson.read(rawJSON)
    val cls = wrapper("#cls").str
    val vid = wrapper("#vid").num
    val rdrOption = VersionedJSON.Readers.get(cls, vid)
    rdrOption match {
      case Some(rdr: ReadsJSON[Versioned]) =>
        Cargo.fromObj(wrapper) match {
          case Some(Cargo.obj) => rdr.fromJSON(wrapper(s"${Cargo.obj}").toString())
//          case Cargo.ar => wrapper(s"${Cargo.ar}").arrOpt match {
//            case Some(arrayJSON: mutable.ArrayBuffer[ujson.Value]) => Some(ArrayJSON.fromJSON(rdr, arrayJSON))
//          }

        }
      case _ => throw UnknownReader(s"VersionedJSON.Reader: Unregistered Class: $cls")
    }
  }

  def apply[T <: Versioned](rawJSON: String)(implicit tag: ClassTag[T]): Option[T] = {
    unwrap(rawJSON) match {
      case Some(currentVersion: T) => Some(currentVersion)
      case Some(oldVersion: OldVersionOf[_]) => upgrade[T](oldVersion)
      //case Some(arrayOf:ArrayJSON[T]) => Some(arrayOf.asInstanceOf[T])
      case o: Any => throw UnknownJSON(s"Can't deserialize unknown version of$tag: $o")
    }
  }
}

case class UnknownJSON(msg: String) extends Exception(msg)
case class UnknownReader(msg: String) extends Exception(msg)
case class UpgradeFailure(msg: String) extends Exception(msg)
case class UnknownVersionedClass(msg: String) extends Exception(msg)