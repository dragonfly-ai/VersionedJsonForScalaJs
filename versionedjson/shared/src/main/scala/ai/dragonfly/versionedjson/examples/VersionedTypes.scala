package ai.dragonfly.versionedjson.examples

import ai.dragonfly.versionedjson.{ReadsVersionedJSON, WritesVersionedJson}
import ai.dragonfly.versionedjson._

object Foo extends ReadsVersionedJSON[Foo] {
  override val vid: Double = 0.3

//  println(this.getClass)
//  override implicit lazy val tag: ClassTag[Foo] = scala.reflect.ClassTag[Foo](Foo(0, 0, 0, 0, false).getClass)

  override val oldVersions: Array[ReadsStaleJSON[_]] = Array[ReadsStaleJSON[_]](
    Foo$0_1,
    Foo$0_2,
  )

  override def fromJSON(rawJSON: String): Option[Foo] = fromObj(ujson.read(rawJSON).obj)

  def fromObj(jsonObj: ujson.Obj): Option[Foo] = for {
    i <- jsonObj("i").numOpt; l <- jsonObj("l").strOpt; f <- jsonObj("f").numOpt; d <- jsonObj("d").numOpt; b <- jsonObj("b").boolOpt
  } yield new Foo(i.toInt, l.toLong, f.toFloat, d, b)

}

/** Foo exemplifies a current version, 0.3, of a versioned case class.
 *  0.3 illustrates how to handle path refactor/change from "com.whatever.Foo" to
 *  "ai.dragonfly.versionedjson.examples.test.Foo".
 *  removes a field: [[s: String]] from the previous version: Foo 0.2.
 *  Where other versioned serialization schemes forbid field removal for
 *  backwards compatibility, this library adopts forward-only compatibility.
 *
 *  To satisfy concerns like forensics, provenance, etc,
 *  users of this versioning scheme should backup old versions of versioned
 *  types with archival techniques.
 *
 *  If design goal changes require the reintroduction of a deleted field,
 *  it can be restored in the upgrade method, spun out into a separate type,
 *  and/or populated from archives of old versions of serialized classes.
 *
 *  @constructor create a Foo instance from various primitive types.
 *  @param i an Int primitive type
 *  @param l a Long primitive type
 *  @param f a Float primitive type
 *  @param d a Double primitive type
 *  @param b a Boolean primitive type
 */

case class Foo( i: Int, l: Long, f: Float, d: Double, b: Boolean ) extends WritesVersionedJson {
  override val vid: Double = Foo.vid
  override def toJSON: String = toObj.render()

  def toObj: ujson.Obj = ujson.Obj("i" -> i, "l" -> l, "f" -> f, "d" -> d, "b" -> b)
  def toVersionedObj: ujson.Obj = VersionedObj(this, toObj)
}

object Foo$0_1 extends ReadsStaleJSON[Foo$0_1] {
  override val cls = "com.whatever.Foo"
  override def fromJSON(rawJSON: String): Option[Foo$0_1] = {
    val jsonObj: ujson.Obj = ujson.read(rawJSON).obj
    for {
      s <- jsonObj("s").strOpt; i <- jsonObj("i").numOpt; l <- jsonObj("l").strOpt; f <- jsonObj("f").numOpt; d <- jsonObj("d").numOpt
    } yield Foo$0_1(s, i.toInt, l.toLong, f.toFloat, d)
  }
}


/** Foo$0_1 exemplifies the first version, 0.1, of the Foo case class.
 * old versions of Foo (and other versioned classes) can not write any json.
 * Instead, they contain upgrade methods that point through the version chain
 * until they reach the current version.
 *  @constructor create a Foo instance from various primitive types.
 *  @param s a String primitive type
 *  @param i an Int primitive type
 *  @param l a Long primitive type
 *  @param f a Float primitive type
 *  @param d a Double primitive type
 */

case class Foo$0_1(s: String, i: Int, l: Long, f: Float, d: Double) extends OldVersionOf[Foo$0_2] {
  override def upgrade: Some[Foo$0_2] = Some( Foo$0_2( s, i, l, f, d, false ) )
}

object Foo$0_2 extends ReadsStaleJSON[Foo$0_2] {
  override val cls = "com.whatever.Foo"

  override def fromJSON(rawJSON: String): Option[Foo$0_2] = {
    val jsonObj: ujson.Obj = ujson.read(rawJSON).obj
    for {
      s <- jsonObj("s").strOpt; i <- jsonObj("i").numOpt; l <- jsonObj("l").strOpt; f <- jsonObj("f").numOpt; d <- jsonObj("d").numOpt; b <- jsonObj("b").boolOpt
    } yield Foo$0_2(s, i.toInt, l.toLong, f.toFloat, d, b)
  }
}

/** Foo$0_2 exemplifies the second version, 0.2, of the Foo case class.
 * Foo 0.2 added the field: [[b: Boolean]] to the Foo type and demonstrates
 * how to add fields to versioned types.
 * old versions of Foo (and other versioned classes) can not write any json.
 * Instead, they contain upgrade methods that point through the version chain
 * until they reach the current version.
 *  @constructor create a Foo instance from various primitive types.
 *  @param s a String primitive type
 *  @param i an Int primitive type
 *  @param l a Long primitive type
 *  @param f a Float primitive type
 *  @param d a Double primitive type
 *  @param b a Boolean primitive type
 */

case class Foo$0_2(s: String, i: Int, l: Long, f: Float, d: Double, b: Boolean) extends OldVersionOf[Foo] {
  override def upgrade: Some[Foo] = Some( new Foo(i, l, f, d, b) )
}

object Bar extends ReadsVersionedJSON[Bar] {
  override val vid: Double = 0.1
  override val oldVersions:Array[ReadsStaleJSON[_]] = Array[ReadsStaleJSON[_]]()

  override def fromJSON(rawJSON: String): Option[Bar] = fromObj(ujson.read(rawJSON).obj)

  def fromObj(jsonObj: ujson.Obj): Option[Bar] = for {
    foo <- VersionedJSON[Foo](jsonObj("foo").obj.render())
  } yield Bar(foo)
}

/*
 *  Bar demonstrates a way to handle nested Versioned objects.
 */

case class Bar(foo: Foo) extends WritesVersionedJson {
  override val vid: Double = Bar.vid
  override def toJSON: String = toObj.toString()

  def toObj: ujson.Obj = ujson.Obj("foo" -> foo.toVersionedObj)
}

object VersionedObj {
  def apply(v: WritesVersionedJson, obj: ujson.Obj): ujson.Obj = ujson.Obj("#vid" -> v.vid, "#cls" -> v.cls, "#obj" -> obj)
}