package ai.dragonfly.types

import microjson.{JsArray, JsObject, JsValue, Json}
import VersionedJSON._
import TestRegistry.registry

import scala.collection.immutable.HashSet
import scala.reflect._
import scala.scalajs.js.annotation.JSExport

object VersionsOfFooSerializations {
  val v0_1 = """{ "#cls": "com.whatever.Foo", "#vid": 0.1, "#val": {"s": "foo", "f": 3.4028235E38, "i": 2147483647, "l": "9223372036854775807", "d": 1.7976931348623157E308}}"""
  val v0_2 = """{"#val": {"s": "foo", "f": 3.4028235E38, "i": 2147483647, "b": true, "l": "9223372036854775807", "d": 1.7976931348623157E308}, "#cls": "com.whatever.Foo", "#vid": 0.2}"""
  val v0_3 = """{"#val": {"f": 3.4028235E38, "i": 2147483647, "b": true, "l": "9223372036854775807", "d": 1.7976931348623157E308}, "#cls": "ai.dragonfly.types.Foo", "#vid": 0.3}"""
}

@JSExport
object Foo extends ReadsVersionedJSON[Foo] {

  override val versionReaders = Map[Double, (JsValue) => Option[Foo]](
    0.1 -> ((jsValue: JsValue) =>  {
      val jsObj: JsObject = jsValue.asInstanceOf[JsObject]
      try {
        Some(
          Foo(
            jsObj.value("i"),
            jsObj.value("l"),
            jsObj.value("f"),
            jsObj.value("d"),
            false
          )
        )
      } catch {
        case _ : Throwable => None
      }
    }),
    0.2 -> ((jsValue: JsValue) =>  {
      val jsObj: JsObject = jsValue.asInstanceOf[JsObject]
      try {
        Some(
          Foo(
            jsObj.value("i"),
            jsObj.value("l"),
            jsObj.value("f"),
            jsObj.value("d"),
            jsObj.value("b")
          )
        )
      } catch {
        case _ : Throwable => None
      }
    }),
    0.3 -> ((jsValue: JsValue) =>  {
      val jsObj: JsObject = jsValue.asInstanceOf[JsObject]
      try {
        Some(
          Foo(
            jsObj.value("i"),
            jsObj.value("l"),
            jsObj.value("f"),
            jsObj.value("d"),
            jsObj.value("b")
          )
        )
      } catch {
        case _ : Throwable => None
      }
    })
  )

}

case class Foo(i: Int, l: Long, f: Float, d: Double, b: Boolean) extends WritesVersionedJSON {
  override def vid: Double = 0.3

  override def toJsValue: JsValue = toJsValue(
    JsObj(
      "i" -> i,
      "l" -> l,
      "f" -> f,
      "d" -> d,
      "b" -> b
    )
  )
}

object VersionsOfBarSerializations {
  val bar0_1 = """{"#cls": "ai.dragonfly.types.Bar", "#vid": 0.1, "#val": {"s": "two guys walked into a bar", "foos": [{"#cls": "ai.dragonfly.types.Foo", "#vid": 0.3, "#val": {"f": 3.4028235E38, "i": 2147483647, "b": false, "l": "9223372036854775807", "d": 1.7976931348623157E308}}, {"#cls": "ai.dragonfly.types.Foo", "#vid": 0.3, "#val": {"f": 3.4028235E38, "i": 2147483647, "b": true, "l": "9223372036854775807", "d": 1.7976931348623157E308}}, {"#cls": "ai.dragonfly.types.Foo", "#vid": 0.3, "#val": {"f": 3.4028235E38, "i": 2147483647, "b": true, "l": "9223372036854775807", "d": 1.7976931348623157E308}}]}}"""
}

@JSExport
object Bar extends ReadsVersionedJSON[Bar] {
  implicit val registry = TestRegistry.registry
  def read0_1And0_2 = (jsValue: JsValue) => {
    val jsObj: JsObject = jsValue.asInstanceOf[JsObject]

    try {
      Some(
        Bar(
          jsObj.value("s"), HashSet[Foo]() ++ jsArrayToArray[Foo](jsObj.value("foos"))
        )
      )
    } catch {
      case e : Throwable =>
        e.printStackTrace()
        None
    }
  }

  override val versionReaders = Map[Double, (JsValue) => Option[Bar]](
    0.1 -> read0_1And0_2,
    0.2 -> read0_1And0_2
  )

}

case class Bar(s: String, foos: Set[Foo]) extends WritesVersionedJSON {
  override def vid: Double = 0.2

  override def toJsValue: JsValue = toJsValue(JsObj(
    "s" -> s,
    "foos" -> JsArray(foos.map(foo => foo.toJsValue).toSeq)
  ))

  override def toString(): String = {
    val sb = new StringBuilder
    s"Bar($s, Array[Foo]( ${foos.foldLeft(sb)((sb, foo) => sb.append(foo).append(" ") )}))"
  }

}