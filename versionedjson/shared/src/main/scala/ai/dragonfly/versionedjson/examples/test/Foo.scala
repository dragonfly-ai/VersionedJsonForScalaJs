package ai.dragonfly.versionedjson.examples.test

import ai.dragonfly.versionedjson.{ReadsVersionedJsonOf, WritesVersionedJson}
import ai.dragonfly.versionedjson.VersionedJson._
import ai.dragonfly.versionedjson._
import microjson.{JsObject, JsValue}


case class Foo(i: Int, l: Long, f: Float, d: Double, b: Boolean) extends WritesVersionedJson {
  override def vid: Double = Foo.vid

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

object Foo extends ReadsVersionedJsonOf[Foo] {

  override def vid: Double = 0.3

  def fromJsValue(jsValue: JsValue): Option[Foo] = {
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
  }

  override val oldVersions = Array[ReadsOldJsonVersion[_]](
    Foo$0_1,
    Foo$0_2
  )

}


object Foo$0_1 extends ReadsOldJsonVersion[Foo$0_1] {

  override val cls = "com.whatever.Foo"

  def fromJsValue(jsValue: JsValue): Option[Foo$0_1] = {
    val jsObj: JsObject = jsValue.asInstanceOf[JsObject]
    try {
      Some(
        Foo$0_1(
          jsObj.value("s"),
          jsObj.value("i"),
          jsObj.value("l"),
          jsObj.value("f"),
          jsObj.value("d")
        )
      )
    } catch {
      case _ : Throwable => None
    }
  }

}

case class Foo$0_1(s: String, i: Int, l: Long, f: Float, d: Double) extends OldVersionOf[Foo$0_2] {
  override def upgrade = Some( Foo$0_2( s, i, l, f, d, false ) )

  override def toJsValue = toJsValue(
    JsObj(
      "s" -> s,
      "i" -> i,
      "l" -> l,
      "f" -> f,
      "d" -> d
    )
  )
}

object Foo$0_2 extends ReadsOldJsonVersion[Foo$0_2] {

  override val cls = "com.whatever.Foo"

  def fromJsValue(jsValue: JsValue): Option[Foo$0_2] = {
    val jsObj: JsObject = jsValue.asInstanceOf[JsObject]
    try {
      Some(
        Foo$0_2(
          jsObj.value("s"),
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
  }

}

case class Foo$0_2(s: String, i: Int, l: Long, f: Float, d: Double, b: Boolean) extends OldVersionOf[Foo] {
  override def upgrade = Some(Foo(i, l, f, d, b))

  override def toJsValue = toJsValue(
    JsObj(
      "s" -> s,
      "i" -> i,
      "l" -> l,
      "f" -> f,
      "d" -> d,
      "b" -> b
    )
  )
}