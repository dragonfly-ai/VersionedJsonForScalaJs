package ai.dragonfly.versionedjson.examples

import ai.dragonfly.versionedjson.VersionedJSON.Cargo
import ai.dragonfly.versionedjson._
import ai.dragonfly.versionedjson.Versioned._

import scala.collection.immutable.TreeMap

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

  override def fromJSON(rawJSON: String)(implicit versions:Array[Version]): Option[Bar] = for {
    obj <- ujson.read(rawJSON).objOpt
    foo <- VersionedElement.fromJSON(obj("foo").render())
  } yield Bar(foo.asInstanceOf[Foo])
}

case class Bar(foo: Foo) extends WritesVersionedJSON[Bar] {
  override def toJSON(implicit versionIndex:VersionIndex): String = s"""{"foo":${foo.toVersionedJSON}}"""
}

/**
 * Wubba demonstrates a way to handle Versioned objects with nested uniform, homogeneous, collections of Versioned objects.
 */

object Wubba extends ReadsVersionedJSON[Wubba] {
  override val version: Version = 0.1

  override val oldVersions:Array[ReadsStaleJSON[_]] = Array[ReadsStaleJSON[_]]()

  override def fromJSON(rawJSON: String)(implicit versions:Array[Version]): Option[Wubba] = for {
    obj <- ujson.read(rawJSON).objOpt
    barsObj <- obj("bars").objOpt
    barsArr <- VersionedArray.fromJSON(barsObj.render())
  } yield Wubba({
    var bars: Seq[Bar] = Seq[Bar]()
    for (b <- barsArr) bars = bars.+:(b.asInstanceOf[Bar])
    bars
  })
}

case class Wubba(bars: Seq[Bar]) extends WritesVersionedJSON[Wubba] {
  override def toJSON(implicit versionIndex:VersionIndex): String = s"""{"bars":${VersionedArray.toJSON(bars:_*)}}"""
}

/**
 * Wubba demonstrates a way to handle Versioned objects with nested diverse, heterogeneous, collections of Versioned objects.
 */

object Woo extends ReadsVersionedJSON[Woo] {
  override val version: Version = 0.1
  override val oldVersions: Array[ReadsStaleJSON[_]] = Array[ReadsStaleJSON[_]]()
  override def fromJSON(rawJSON: String)(implicit versions: Array[Version]): Option[Woo] = for {
    obj <- ujson.read(rawJSON).objOpt
    shapesObj <- obj("shapes").objOpt
    shapesArr <- VersionedArray.fromJSON(shapesObj.render())
  } yield Woo({
    var s: Seq[Shape] = Seq[Shape]()
    for (b <- shapesArr) s = s.+:(b.asInstanceOf[Shape])
    s
  })
}

case class Woo(shapes: Seq[Shape]) extends WritesVersionedJSON[Woo] {
  override def toJSON(implicit versionIndex:VersionIndex): String = s"""{"shapes":${VersionedArray.toJSON(shapes:_*)}}"""
}

object Point2D extends ReadsVersionedJSON[Point2D] {
  override val version: Version = 0.1
  override val oldVersions: Array[ReadsStaleJSON[_]] = Array[ReadsStaleJSON[_]]()
  override def fromJSON(rawJSON: String)(implicit versions: Array[Version]): Option[Point2D] = for {
    arr <- ujson.read(rawJSON).arrOpt
    x <- arr(0).numOpt
    y <- arr(1).numOpt
  } yield Point2D(x.toDouble, y.toDouble)
}

case class Point2D(x:Double = 0.0, y: Double = 0.0) extends WritesVersionedJSON[Point2D] {
  override def toJSON(implicit versionIndex: VersionIndex): String = s"[$x,$y]"
}

object Color extends ReadsVersionedJSON[Color] {
  override val version: Version = 0.1
  override val oldVersions: Array[ReadsStaleJSON[_]] = Array[ReadsStaleJSON[_]]()
  override def fromJSON(rawJSON: String)(implicit versions: Array[Version]): Option[Color] = for {
    obj <- ujson.read(rawJSON).objOpt
    r <- obj("r").numOpt
    g <- obj("g").numOpt
    b <- obj("b").numOpt
  } yield new Color(r.toInt, g.toInt, b.toInt)
}


case class Color(red: Int, green: Int, blue: Int) extends WritesVersionedJSON[Color] {
  override def toJSON(implicit versionIndex: VersionIndex): String = s"""{"r":$red,"g":$green,"b":$blue}"""
}

trait Shape extends WritesVersionedJSON[Shape] {
  val points: Array[Point2D]
  val position: Point2D
  val color:Color
}

object Triangle extends ReadsVersionedJSON[Triangle] {
  override val version: Version = 0.1
  override val oldVersions: Array[ReadsStaleJSON[_]] = Array[ReadsStaleJSON[_]]()
  override def fromJSON(rawJSON: String)(implicit versions: Array[Version]): Option[Triangle] = for {
    obj <- ujson.read(rawJSON).objOpt
    points <- obj("points").objOpt
    arr <- VersionedArray.fromJSON(points.render())
    position <- VersionedElement.fromJSON(obj("position").render())
    color <- VersionedElement.fromJSON(obj("color").render())
  } yield Triangle(arr(0).asInstanceOf[Point2D], arr(1).asInstanceOf[Point2D], arr(2).asInstanceOf[Point2D], position.asInstanceOf[Point2D], color.asInstanceOf[Color])
}

case class Triangle(p1: Point2D, p2: Point2D, p3: Point2D, override val position: Point2D = Point2D(), override val color:Color) extends Shape {
  override val points:Array[Point2D] = Array[Point2D](p1, p2, p3)
  override def toJSON(implicit versionIndex: VersionIndex): String = s"""{"points":${VersionedArray.toJSON(p1, p2, p3)}, "position":${position.toVersionedJSON}, "color":${color.toVersionedJSON}}"""
}


object Rectangle extends ReadsVersionedJSON[Rectangle] {
  override val version: Version = 0.1
  override val oldVersions: Array[ReadsStaleJSON[_]] = Array[ReadsStaleJSON[_]]()
  override def fromJSON(rawJSON: String)(implicit versions: Array[Version]): Option[Rectangle] = for {
    obj <- ujson.read(rawJSON).objOpt
    width <- obj("w").numOpt
    height <- obj("h").numOpt
    position <- VersionedElement.fromJSON(obj("position").render())
    color <- VersionedElement.fromJSON(obj("color").render())
  } yield Rectangle(width, height, position.asInstanceOf[Point2D], color.asInstanceOf[Color])
}

case class Rectangle(width: Double, height: Double, override val position: Point2D = Point2D(), override val color:Color) extends Shape {
  override val points:Array[Point2D] = Array[Point2D]( Point2D(width, height), Point2D(width, -height), Point2D(-width, -height), Point2D(-width, height) )
  override def toJSON(implicit versionIndex: VersionIndex): String = s"""{"w":$width,"h":$height,"position":${position.toVersionedJSON},"color":${color.toVersionedJSON}}"""
}

object Square extends ReadsVersionedJSON[Square] {
  override val version: Version = 0.1
  override val oldVersions: Array[ReadsStaleJSON[_]] = Array[ReadsStaleJSON[_]]()
  override def fromJSON(rawJSON: String)(implicit versions: Array[Version]): Option[Square] = for {
    obj <- ujson.read(rawJSON).objOpt
    length <- obj("l").numOpt
    position <- VersionedElement.fromJSON(obj("position").render())
    color <- VersionedElement.fromJSON(obj("color").render())
  } yield Square(length, position.asInstanceOf[Point2D], color.asInstanceOf[Color])
}

case class Square(length: Double, override val position: Point2D = Point2D(), override val color:Color) extends Shape {
  override val points:Array[Point2D] = {
    val hsl: Double = length/2.0
    Array[Point2D]( Point2D(hsl, hsl), Point2D(hsl, -hsl), Point2D(-hsl, -hsl), Point2D(-hsl, hsl) )
  }
  override def toJSON(implicit versionIndex: VersionIndex): String = s"""{"l":$length,"position":${position.toVersionedJSON},"color":${color.toVersionedJSON}}"""
}


object Circle extends ReadsVersionedJSON[Circle] {
  override val version: Version = 0.1
  override val oldVersions: Array[ReadsStaleJSON[_]] = Array[ReadsStaleJSON[_]]()
  override def fromJSON(rawJSON: String)(implicit versions: Array[Version]): Option[Circle] = for {
    obj <- ujson.read(rawJSON).objOpt
    radius <- obj("r").numOpt
    position <- VersionedElement.fromJSON(obj("position").render())
    color <- VersionedElement.fromJSON(obj("color").render())
  } yield Circle(radius, position.asInstanceOf[Point2D], color.asInstanceOf[Color])
}

case class Circle(radius: Double, override val position: Point2D = Point2D(), override val color:Color) extends Shape {
  override val points:Array[Point2D] = new Array[Point2D](32)
  for (p <- 0 until 8) {
    val x = radius * Math.cos(p * Math.PI / 16.0)
    val y = radius * Math.sin(p * Math.PI / 16.0)
    points(p) = Point2D(y, x)
    points(p + 8) = Point2D(x, -y)
    points(p + 16) = Point2D(-y, -x)
    points(p + 24) = Point2D(-x, y)
  }
  override def toJSON(implicit versionIndex: VersionIndex): String = s"""{"r":$radius,"position":${position.toVersionedJSON},"color":${color.toVersionedJSON}}"""
}