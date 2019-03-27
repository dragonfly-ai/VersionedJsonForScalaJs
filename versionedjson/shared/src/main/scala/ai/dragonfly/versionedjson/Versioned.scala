package ai.dragonfly.versionedjson

import scala.collection.mutable.HashMap
import scala.reflect.ClassTag
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}


/**
  * Traits for classes and companion objects
  */

object Versioned {

  def toJSON(v: Versioned)(payload:String): String = ujson.Obj(
    "#vid" -> v.vid,
    "#cls" -> v.getClass.getName,
    "#obj" -> ujson.read(payload)
  ).render()

  def fromJSON[T <: Versioned](jsonStr: ujson.Readable): Option[T] = {
    val wrapper = ujson.read(jsonStr)
    val cls = wrapper("#cls").str
    val vid = wrapper("#vid").num
    val rdrOption = VersionedJsonReaderRegistry.get(cls, vid)
    println(s"cls = $cls vid = $vid rdrOption = $rdrOption")
    rdrOption match {
      case Some(rdr: ReadsJSON[T]) => rdr.fromJSON(wrapper("#obj"))
      case _ => None
    }
  }
}

trait Versioned {
  val vid: Double
}

/**
  * traits for classes
  */

trait WritesVersionedJson extends Versioned {
  def toJSON: String
}

trait OldVersionOf[T] extends WritesVersionedJson {
  val vid: Double = VersionedJson.getVersionIdFromClassName(this.getClass.getName)
  def upgrade: Option[T]
}


/**
  * traits for companion objects
  */

trait ReadsJSON[T <: Versioned] extends Versioned {
  val cls: String
  def fromJSON(value: ujson.Readable): Option[T]
}

trait ReadsVersionedJSON[T <: Versioned] extends ReadsJSON[T] {
  override val cls: String = {
    val temp = this.getClass.getName
    temp.substring(0, temp.length - 1)
  }
  val oldVersions: Array[ReadsStaleJSON[_]]
}

trait ReadsStaleJSON[T <: Versioned] extends ReadsJSON[T] {
  override val vid: Double = VersionedJson.getVersionIdFromClassName(this.getClass.getName)
}

/*
  Versioned JSON serialization registry
 */

@JSExportTopLevel("VersionedJson")
object VersionedJson {

  def getVersionIdFromClassName(oldClassName: String): Double = {
    var temp = oldClassName.split("[$]")(1)
    temp = temp.replace('_', '.')
    java.lang.Double.parseDouble(temp)
  }


  def upgrade[T <: WritesVersionedJson](o: OldVersionOf[_])(implicit tag: ClassTag[T]): Option[T] = {
    o.upgrade match {
      case Some(ov: OldVersionOf[_]) => upgrade(ov)
      case Some(nv: T) => Some(nv)
      case _ => None
    }
  }

  @JSExport
  def fromJSON[T <: WritesVersionedJson](jsonStr: String)(implicit tag: ClassTag[T]): Option[T] = {
    Versioned.fromJSON[T](jsonStr) match {
      case Some(obj: T) => Some(obj)
      case Some(obj: OldVersionOf[_]) => upgrade[T](obj)
      case o: Any => None
    }
  }
}

object VersionedJsonReaders {

  def apply(readers: ReadsVersionedJSON[_]*) = synchronized {
    for (r <- readers) {
      // println(s"registered reader ${r.cls}")
      VersionedJsonReaderRegistry.put(r)
      for (ov <- r.oldVersions) {
        // println(s"registered old version reader ${ov.cls}")
        VersionedJsonReaderRegistry.put(ov)
      }
    }
  }

  def apply(cls: String, vid: Double): Option[ReadsJSON[_]] = VersionedJsonReaderRegistry.get(cls, vid)

  override def toString: String = VersionedJsonReaderRegistry.toString
}

object VersionedJsonReaderRegistry {
  private val registry = HashMap[String, HashMap[Double, ReadsJSON[_]]]()

  def put (reader: ReadsJSON[_]): Unit = {
    val hm: HashMap[Double, ReadsJSON[_]] = registry.get(reader.cls) match {
      case Some(hm) => hm
      case None =>
        val temp = new HashMap[Double, ReadsJSON[_]]()
        registry.put(reader.cls, temp)
        temp
    }

    hm.put(reader.vid, reader)
  }

  def get (cls: String, vid: Double): Option[ReadsJSON[_]] = {
    println(s"registry.get($cls) -> ${registry.get(cls)}")
    for {
      hm <- registry.get(cls)
      reader <- hm.get(vid)
    } yield reader
  }

  override def toString: String = this.registry.toString()
}