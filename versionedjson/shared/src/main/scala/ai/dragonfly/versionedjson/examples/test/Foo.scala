package ai.dragonfly.versionedjson.examples.test

import ai.dragonfly.distributed.Sn0wflake
import ai.dragonfly.versionedjson.{ReadsVersionedJSON, WritesVersionedJson}
import ai.dragonfly.versionedjson._
import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import scala.collection.mutable


case class Foo(i: Int, l: Long, f: Float, d: Double, b: Boolean) extends WritesVersionedJson {
  override val vid: Double = Foo.vid
  override def toJSON: String = Versioned.toJSON(this)(write(this))
}

object Foo extends ReadsVersionedJSON[Foo] {
  override val vid: Double = 0.3
  implicit val rw: RW[Foo] = macroRW

  override val oldVersions: Array[ReadsStaleJSON[_]] = Array[ReadsStaleJSON[_]](
    Foo$0_1,
    Foo$0_2
  )

  override def fromJSON(value: ujson.Readable): Option[Foo] = Some(read[Foo](value))
}

object Foo$0_1 extends ReadsStaleJSON[Foo$0_1] {
  override val cls = "com.whatever.Foo"
  implicit val rw: RW[Foo$0_1] = macroRW
  override def fromJSON(value: ujson.Readable): Option[Foo$0_1] = Some(read[Foo$0_1](value))
}

case class Foo$0_1(s: String, i: Int, l: Long, f: Float, d: Double) extends OldVersionOf[Foo$0_2] {
  override def upgrade = Some( Foo$0_2( s, i, l, f, d, false ) )
  override def toJSON: String = Versioned.toJSON(this)(write(this))
}

object Foo$0_2 extends ReadsStaleJSON[Foo$0_2] {
  override val cls = "com.whatever.Foo"
  implicit val rw: RW[Foo$0_2] = macroRW
  override def fromJSON(value: ujson.Readable): Option[Foo$0_2] = Some(read[Foo$0_2](value))
}

case class Foo$0_2(s: String, i: Int, l: Long, f: Float, d: Double, b: Boolean) extends OldVersionOf[Foo] {
  override def upgrade = Some(Foo(i, l, f, d, b))
  override def toJSON: String = Versioned.toJSON(this)(write(this))
}
/*
case class Bar(m: mutable.HashMap[Sn0wflake, Foo], a: Array[Foo]) extends WritesVersionedJson {
  override val vid: Double = Bar.vid
  override def toJSON: String = Versioned.toJSON(this)(write(this))
}

object Bar extends ReadsVersionedJSON[Bar] {
  override val vid: Double = 0.1
  override val oldVersions:Array[ReadsStaleJSON[_]] = Array[ReadsStaleJSON[_]]()
  implicit val rw: RW[Bar] = macroRW
}
*/