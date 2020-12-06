package ai.dragonfly.versionedjson

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * Traits for classes and companion objects
  */

trait Versioned {
  val vid: Double
  val cls: String = this.getClass.getName
}

/**
  * traits for classes
  */

trait WritesVersionedJson extends Versioned {
  def toJSON: String
  def toVersionedJSON: String = VersionedJSON(this)(toJSON)
}

trait OldVersionOf[T] extends Versioned {
  val vid: Double = VersionedJSON.getVersionIdFromClassName(this.getClass.getName)
  def upgrade: Option[T]
}


/**
  * traits for companion objects
  */

/*
Internal use only.
 */
sealed trait ReadsJSON[T <: Versioned] extends Versioned {
  val cls: String
  implicit val tag: ClassTag[T] = ClassTag(this.getClass)
  def fromJSON(rawJSON: String): Option[T]
  // fromJSON(rawJSON: String): Option[T] = VersionedJSON.fromJSON[T](rawJSON)
}

/*
  Meant only for the current version of the class.
 */

trait ReadsVersionedJSON[T <: Versioned] extends ReadsJSON[T] {
  override val cls: String = {
    val temp = this.getClass.getName
    temp.substring(0, temp.length - 1)
  }
  val oldVersions: Array[ReadsStaleJSON[_]]
  def fromVersionedJSON: String => Option[T] = { VersionedJSON.Readers(this); (rjsn:String) => VersionedJSON[T](rjsn) }
}

/*
  Only intended for past versions of classes.
 */
trait ReadsStaleJSON[T <: Versioned] extends ReadsJSON[T] {
  override val vid: Double = VersionedJSON.getVersionIdFromClassName(this.getClass.getName)
}

/*
  Versioned JSON serialization registry
*/

object VersionedJSON {

  def getVersionIdFromClassName(className: String): Double = java.lang.Double.parseDouble( className.split("[$]")(1).replace('_', '.') )

  def upgrade[T <: Versioned](o: OldVersionOf[_])(implicit tag: ClassTag[T]): Option[T] = {
    o.upgrade match {
      case Some(ov: OldVersionOf[_]) => upgrade(ov)
      case Some(nv: T) => Some(nv)
      case _ => None
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
      //println(s"registry.get($cls) -> ${registry.get(cls)}")
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
    //println(s"cls = $cls vid = $vid rdrOption = $rdrOption")
    rdrOption match {
      case Some(rdr: ReadsJSON[Versioned]) =>
        //println(s"found valid reader: $rdr")
        rdr.fromJSON(wrapper("#obj").toString()) // may parse payload json twice.
      case _ =>
        System.out.println(s"VersionedJSON.Reader: Unregistered Class: $cls");
        None
    }
  }

  def apply[T <: Versioned](rawJSON: String)(implicit tag: ClassTag[T]): Option[T] = {
    unwrap(rawJSON) match {
      case Some(obj: WritesVersionedJson) =>
        //println(s"Found Current version: $obj")
        Some(obj.asInstanceOf[T])
      case Some(obj: OldVersionOf[_]) =>
        //println(s"Found Old version: $obj")
        upgrade[T](obj)
      case o: Any =>
        //println(s"Found something else: $o")
        None
    }
  }
}
