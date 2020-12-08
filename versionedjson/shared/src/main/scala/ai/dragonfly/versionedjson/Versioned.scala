package ai.dragonfly.versionedjson

import ai.dragonfly.versionedjson.native
import native.Versioned

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * traits for classes
  */
trait WritesVersionedJson extends Versioned {
  def toJSON: String
  def toVersionedJSON: String = VersionedJSON(this)(toJSON)
  override val cls: String = this.getClass.getName
}

trait OldVersionOf[T] extends Versioned {
  val vid: Double = VersionedJSON.getVersionIdFromClassName(this.getClass.getName)
  override val cls: String = this.getClass.getName
  def upgrade: Option[T]
}


/**
  * traits for companion objects
  */

sealed trait ReadsJSON[T <: Versioned] extends Versioned {
  val cls: String
  def fromJSON(rawJSON: String): Option[T]

  implicit val tag: ClassTag[T] = native.ClassTag[T](this.getClass.getName.split("\\$")(0))
}

/**
  Meant only for the current version of the class.
 */

trait ReadsVersionedJSON[T <: Versioned] extends ReadsJSON[T] {
  override lazy val cls: String = tag.toString()

  val oldVersions: Array[ReadsStaleJSON[_]]

  lazy val fromVersionedJSON: String => Option[T] = {
    VersionedJSON.Readers(this)  // only register readers the first time this method is called.
    (rawJSON:String) => VersionedJSON[T](rawJSON)
  }
}

/**
  Only intended for past versions of classes.
 */
trait ReadsStaleJSON[T <: Versioned] extends ReadsJSON[T] {
  override val vid: Double = VersionedJSON.getVersionIdFromClassName(this.getClass.getName)
}

/**
  VersionedJSON serialization registry
*/

object VersionedJSON {

  def getVersionIdFromClassName(className: String): Double = java.lang.Double.parseDouble( className.split("[$]")(1).replace('_', '.') )

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
      val hm: mutable.HashMap[Double, ReadsJSON[_]] = registry.get(reader.cls) match {
        case Some(hm) => hm
        case None =>
          val temp = new mutable.HashMap[Double, ReadsJSON[_]]()
          registry.put(reader.cls, temp)
          temp
      }
      hm.put(reader.vid, reader)
    }

    def get (cls: String, vid: Double): Option[ReadsJSON[_]] = {
      for {
        hm <- registry.get(cls)
        reader <- hm.get(vid)
      } yield reader
    }

    def apply(readers: ReadsVersionedJSON[_]*):Readers.type = synchronized {
      for (r <- readers) {
        println(s"Registered Reader ${r.cls}")
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

  def apply(v: Versioned)(payload:String): String = s"""{"#vid":${v.vid},"#cls":"${v.cls}","#obj":$payload}"""

  def unwrap(rawJSON: String): Option[Versioned] = {
    val wrapper = ujson.read(rawJSON)
    val cls = wrapper("#cls").str
    val vid = wrapper("#vid").num
    val rdrOption = VersionedJSON.Readers.get(cls, vid)
    rdrOption match {
      case Some(rdr: ReadsJSON[_]) => rdr.fromJSON(wrapper("#obj").toString())
      case _ => throw UnknownReader(s"VersionedJSON.Reader: Unregistered Class: $cls")
    }
  }

  def apply[T <: Versioned](rawJSON: String)(implicit tag: ClassTag[T]): Option[T] = {
    unwrap(rawJSON) match {
      case Some(obj: WritesVersionedJson) => Some(obj.asInstanceOf[T])
      case Some(obj: OldVersionOf[_]) => upgrade[T](obj)
      case o: Any => throw UnknownJSON(s"Can't deserialize unknown version of$tag: $o")
    }
  }
}

case class UnknownJSON(msg: String) extends Exception(msg)
case class UnknownReader(msg: String) extends Exception(msg)
case class UpgradeFailure(msg: String) extends Exception(msg)
case class UnknownVersionedClass(msg: String) extends Exception(msg)