package ai.dragonfly.versionedjson.examples

import ai.dragonfly.versionedjson.VersionedJSON.Cargo
import ai.dragonfly.versionedjson._
import ai.dragonfly.versionedjson.Versioned._

import scala.+:

object Foo extends ReadsVersionedJSON[Foo] {

  // From the Reader perspective, we need only specify the version id:
  override val version: Version = 0.3

  override val oldVersions: Array[ReadsStaleJSON[_]] = Array[ReadsStaleJSON[_]](
    Foo$0_2,
    Foo$0_1
  )

  override def fromJSON(rawJSON: String)(implicit versions:Array[Version]): Option[Foo] = fromObj(ujson.read(rawJSON).obj)

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

case class Foo( i: Int, l: Long, f: Float, d: Double, b: Boolean ) extends WritesVersionedJSON[Foo] {
  override def toJSON(implicit versionIndex:VersionIndex): String = ujson.Obj("i" -> i, "l" -> l, "f" -> f, "d" -> d, "b" -> b).render()
}

/**
 * older versions of Foo:
 * Instead of updating old versions of foo in place, we rename them and create a new class definition for the current version.
 * The old version starts with its original class name, then appends its version number after a $ which serves as a separator.
 * Because class names can not have '.' in them, we substitute the underscore '_'.
 */

object Foo$0_2 extends ReadsStaleJSON[Foo$0_2] {
  override val version: Version = "com.whatever.Foo"

  override def fromJSON(rawJSON: String)(implicit versions:Array[Version]): Option[Foo$0_2] = {
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
  override def upgrade: Some[Foo] = Some( Foo(i, l, f, d, b) )
}

object Foo$0_1 extends ReadsStaleJSON[Foo$0_1] {
  // some class names get refactored from one package to another, or renamed between versions.
  // For Old Version readers we need only specify the old fully qualified class name.  The version id gets inferred from
  // the class name.

  override val version: Version = "com.whatever.Foo"

  override def fromJSON(rawJSON: String)(implicit versions:Array[Version]): Option[Foo$0_1] = {
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

/**
 *  Bar demonstrates a way to handle nested Versioned objects.
 */

object Bar extends ReadsVersionedJSON[Bar] {
  override val version: Version = 0.1
  override val oldVersions:Array[ReadsStaleJSON[_]] = Array[ReadsStaleJSON[_]]()

  override def fromJSON(rawJSON: String)(implicit versions:Array[Version]): Option[Bar] = {
    for {
      obj <- ujson.read(rawJSON).objOpt
      foo <- VersionedElement.fromJSON(obj("foo").render())
    } yield Bar(foo.asInstanceOf[Foo])
  }
}

case class Bar(foo: Foo) extends WritesVersionedJSON[Bar] {
  override def toJSON(implicit versionIndex:VersionIndex): String = s"""{"foo":${foo.toVersionedJSON}}"""
}

/**
 * Wubba demonstrates a way to handle collections of Versioned objects nested in versioned objects.
 */

object Wubba extends ReadsVersionedJSON[Wubba] {

  override val version: Version = 0.1

  override val oldVersions:Array[ReadsStaleJSON[_]] = Array[ReadsStaleJSON[_]]()

  override def fromJSON(rawJSON: String)(implicit versions:Array[Version]): Option[Wubba] = {
    for {
      obj <- ujson.read(rawJSON).objOpt
      barsObj <- obj("bars").objOpt
    } yield Wubba({
      val barsArr = VersionedArray.fromJSON(barsObj.render())
      var bars: Seq[Bar] = Seq[Bar]()
      for (b <- barsArr) bars = bars.+:(b.asInstanceOf[Bar])
      bars
    })
  }

}

case class Wubba(bars: Seq[Bar]) extends WritesVersionedJSON[Wubba] {

  override def toJSON(implicit versionIndex:VersionIndex): String = s"""{"bars":${VersionedArray.toJSON(bars:_*)}}"""

}
