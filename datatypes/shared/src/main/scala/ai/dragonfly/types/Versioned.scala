package ai.dragonfly.types

import microjson.{JsValue, _}

import scala.collection.mutable.HashMap
import scala.reflect.ClassTag
import scala.scalajs.js.annotation.JSExport

/**
  * trait for classes
  */

trait Versioned {
  def vid: Double
}

trait WritesJSON {
  def toJsValue:JsValue
  def JSON : String = Json.write(this.toJsValue)
}

trait WritesVersionedJSON extends Versioned with WritesJSON {
  import VersionedJSON._

  def toJsValue(value: JsObject): JsValue = {
    JsObj(
      "#cls" -> this.getClass.getName,
      "#vid" -> this.vid,
      "#val" -> value
    )
  }
}

/**
  * trait for companion objects
  */

trait ReadsJSON {
  def fromJSON(jsonString: String): Option[WritesJSON]
}

case class VersionInfo(className: String, versionId: Double, value: JsValue)

trait ReadsVersionedJSON[T] {

  val versionReaders: Map[Double, (JsValue) => Option[WritesVersionedJSON]]

}

/*
  Versioned JSON serialiation registry
 */

@JSExport("VersionedJSON")
object VersionedJSON {

  // Implicits:
  //implicit def jsonStringToJsValue(jsonString: String): JsValue = Json.read(jsonString)

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

  def jsArrayToArray[T <: WritesVersionedJSON](jsArr: JsArray)(implicit tag: ClassTag[T], registry: VersionedJSONReaders): Array[T] = {

    val output = new Array[T](jsArr.value.size)

    var i = 0
    for (f <- jsArr.value) {
      VersionedJSON.fromJsValue(f) match {
        case Some(obj) => output(i) = obj.asInstanceOf[T]
        case _ => throw new Exception( "Could not interperet: " + Json.write(f))
      }
      i = i + 1
    }

    output
  }

  implicit def hashMapToJsObject(jsObj: Map[String, JsValue]): JsObject = JsObject(jsObj)


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

  def fromJsValue[T <: WritesVersionedJSON](jsv: JsValue)(implicit tag: ClassTag[T], registry: VersionedJSONReaders): Option[T] = {
    for {
      vi <- getVersionInfo(jsv.asInstanceOf[JsObject])
      readerObj <- registry(vi.className)
      reader <- readerObj.versionReaders.get(vi.versionId)
      payload <- reader(vi.value)
    } yield payload.asInstanceOf[T]

  }

  @JSExport
  def fromJSON[T <: WritesVersionedJSON](jsonText: String)(implicit tag: ClassTag[T], registry: VersionedJSONReaders): Option[T] = fromJsValue(Json.read(jsonText))
}

class VersionedJSONReaders() {

  private val registry = HashMap[String, ReadsVersionedJSON[_]]()

  def registerVersionedJsonReader(params: (String, ReadsVersionedJSON[_])*) = synchronized {
    for ((className, reader) <- params) registry.put(className, reader)
  }

  def apply(className: String): Option[ReadsVersionedJSON[_]] = registry.get(className)
}