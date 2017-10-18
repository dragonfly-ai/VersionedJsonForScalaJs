package ai.dragonfly.versionedjson

import microjson.{JsValue, _}

import scala.collection.mutable.HashMap
import scala.reflect.ClassTag
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/**
  * Traits for classes and companion objects
  */

trait Versioned {
  def vid: Double
}

/**
  * traits for classes
  */

trait WritesJson {
  def toJsValue:JsValue
  def JSON : String = Json.write(this.toJsValue)
}

trait WritesVersionedJson extends Versioned with WritesJson {
  import VersionedJson._

  def toJsValue(value: JsObject): JsValue = {
    JsObj(
      "#cls" -> this.getClass.getName,
      "#vid" -> this.vid,
      "#val" -> value
    )
  }
}

trait OldVersionOf[T] extends WritesVersionedJson {
  val vid: Double = VersionedJson.getVersionIdFromClassName(this.getClass.getName)
  def upgrade: Option[T]
}


/**
  * traits for companion objects
  */

trait ReadsJsonOf[T <: Versioned] extends Versioned {
  //def fromJson(jsonString: String): Option[T]
  def cls: String
  def fromJsValue(jsvalue: JsValue): Option[T]
}

case class VersionInfo(cls: String, vid: Double, value: JsValue)

trait ReadsVersionedJsonOf[T <: Versioned] extends ReadsJsonOf[T] {
  def cls: String = this.getClass.getName.split("[$]")(0)
  val oldVersions: Array[ReadsOldJsonVersion[_]] //(JsValue) => Option[T]]
}

trait ReadsOldJsonVersion[T <: Versioned] extends ReadsJsonOf[T] {
  def vid: Double = VersionedJson.getVersionIdFromClassName(this.getClass.getName)
}

/*
  Versioned JSON serialiation registry
 */

@JSExportTopLevel("VersionedJson")
object VersionedJson {

  // Implicits:
  // implicit def jsonStringToJsValue(jsonString: String): JsValue = Json.read(jsonString)

  implicit def booleanToJsBoolean(b: Boolean): JsValue = if (b) JsTrue else JsFalse
  implicit def intToJsNumber(i: Int): JsValue = JsNumber(i.toString)
  implicit def longToJsString(l: Long): JsValue = JsString(l.toString)
  implicit def floatToJsNumber(f: Float): JsValue = JsNumber(f.toString)
  implicit def doubleToJsNumber(d: Double): JsValue = JsNumber(d.toString)
  implicit def stringToJsString(s: String): JsValue = JsString(s)

  implicit def jsValueToBoolean(jsv: JsValue): Boolean = jsv.asInstanceOf[JsBoolean].value
  implicit def jsValueToInt(jsv: JsValue): Int = java.lang.Integer.parseInt(jsv.asInstanceOf[JsNumber].value)
  implicit def jsValueToFloat(jsv: JsValue): Float = java.lang.Float.parseFloat(jsv.asInstanceOf[JsNumber].value)
  implicit def jsValueToDouble(jsv: JsValue): Double = java.lang.Double.parseDouble(jsv.asInstanceOf[JsNumber].value)
  implicit def jsValueToLong(jsv: JsValue): Long = java.lang.Long.parseLong(jsv.asInstanceOf[JsString].value)
  implicit def jsValueToString(jsv: JsValue): String = jsv.asInstanceOf[JsString].value
  implicit def jsValueToJsArray(jsv: JsValue): JsArray = jsv.asInstanceOf[JsArray]

  def jsArrayToArray[T <: WritesVersionedJson](jsArr: JsArray)(implicit tag: ClassTag[T], registry: VersionedJsonReaders): Array[T] = {

    val output = new Array[T](jsArr.value.size)

    var i = 0
    for (f <- jsArr.value) {
      VersionedJson.fromJsValue(f) match {
        case Some(obj) => output(i) = obj.asInstanceOf[T]
        case _ => throw new Exception( "Could not interpret: " + Json.write(f))
      }
      i = i + 1
    }

    output
  }

  implicit def hashMapToJsObject(jsObj: Map[String, JsValue]): JsObject = JsObject(jsObj)

  def getVersionIdFromClassName(oldClassName: String): Double = {
//    println("Full: " + oldClassName)
    var temp = oldClassName.split("[$]")(1)
//    println("First: " + temp)
    temp = temp.replace('_', '.')
//    println("Mutated: " + temp)
    java.lang.Double.parseDouble(temp)
  }


  def upgrade[T <: WritesVersionedJson](o: OldVersionOf[_])(implicit tag: ClassTag[T]): Option[T] = {
    o.upgrade match {
      case Some(ov: OldVersionOf[_]) => upgrade(ov)
      case Some(nv: T) => Some(nv)
      case _ => None
    }
  }

  def getVersionInfo(jsObj: JsObject): Option[VersionInfo] = {

    for {
      cls <- jsObj.value.get("#cls")
      version <- jsObj.value.get("#vid")
      jsValue <- jsObj.value.get("#val")
    } yield VersionInfo(
      cls.value.asInstanceOf[String],
      java.lang.Double.parseDouble(version.asInstanceOf[JsNumber].value),
      jsValue
    )
  }

  def fromJsValue[T <: Versioned](jsv: JsValue)(implicit tag: ClassTag[T]): Option[T] = {
    for {
      vi <- getVersionInfo(jsv.asInstanceOf[JsObject])
      readerObj <- VersionedJsonReadersRegistry.get(vi.cls, vi.vid)
      payload <- readerObj.fromJsValue(vi.value)
    } yield payload.asInstanceOf[T] //{ println(vi); println(readerObj); println(payload); payload.asInstanceOf[T] }

  }

  @JSExport
  def fromJson[T <: WritesVersionedJson](jsonText: String)(implicit tag: ClassTag[T]): Option[T] = {
    fromJsValue[T](Json.read(jsonText)) match {
      case Some(obj: T) => Some(obj)
      case Some(obj: OldVersionOf[_]) => upgrade[T](obj)
      case _ => None
    }

  }
}

class VersionedJsonReaders() {

  def registerVersionedJsonReader(readers: ReadsVersionedJsonOf[_]*) = synchronized {

    for (r <- readers) {
      VersionedJsonReadersRegistry.put(r)
      for (ov <- r.oldVersions) {
        VersionedJsonReadersRegistry.put(ov)
      }
    }
  }

  def apply(cls: String, vid: Double): Option[ReadsJsonOf[_]] = VersionedJsonReadersRegistry.get(cls, vid)

}

object VersionedJsonReadersRegistry {
  private val registry = HashMap[String, HashMap[Double, ReadsJsonOf[_]]]()

  def put (reader: ReadsJsonOf[_]): Unit = {
    val hm: HashMap[Double, ReadsJsonOf[_]] = registry.get(reader.cls) match {
      case Some(hm) => hm
      case None =>
        val temp = new HashMap[Double, ReadsJsonOf[_]]()
        registry.put(reader.cls, temp)
        temp
    }

    hm.put(reader.vid, reader)
  }

  def get (cls: String, vid: Double): Option[ReadsJsonOf[_]] = {
    for {
      hm <- registry.get(cls)
      reader <- hm.get(vid)
    } yield reader

  }
}