package ai.dragonfly.versionedjson.examples

import ai.dragonfly.versionedjson.{examples, _}
import ai.dragonfly.versionedjson.Versioned._

import scala.collection.immutable

object Foo extends ReadsVersionedJSON[Foo] {

  // From the Reader perspective, we need only specify the version id:
  override val version: Version = 0.3

  override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]](
    Foo$0_2,
    Foo$0_1
  )

  override def fromJSON(rawJSON: String)(implicit readers:ReaderCache): Option[Foo] = {
    import Primitive._
    for {
      obj <- ujson.read(rawJSON).objOpt
      i <- int.fromJSON(obj("i").render())
      l <- long.fromJSON(obj("l").render())
      f <- float.fromJSON(obj("f").render())
      d <- double.fromJSON(obj("d").render())
      b <- boolean.fromJSON(obj("b").render())
    } yield new Foo(i.p, l.p, f.p, d.p, b.p)
  }

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
  override def toJSON(implicit versionIndex:VersionIndex): String = {
    ujson.Obj("i" -> i, "l" -> l, "f" -> f, "d" -> d, "b" -> b).render()
  }
}

/**
 * older versions of Foo:
 * Instead of updating old versions of foo in place, we rename them and create a new class definition for the current version.
 * The old version starts with its original class name, then appends its version number after a $ which serves as a separator.
 * Because class names can not have '.' in them, we substitute the underscore '_'.
 */

object Foo$0_2 extends ReadsStaleJSON[Foo$0_2] {
  override val version: Version = "com.whatever.Foo"

  override def fromJSON(rawJSON: String)(implicit readers:ReaderCache): Option[Foo$0_2] = {
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
  // the current class name.

  override val version: Version = "com.whatever.Foo"

  override def fromJSON(rawJSON: String)(implicit readers:ReaderCache): Option[Foo$0_1] = {
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
  override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()

  override def fromJSON(rawJSON: String)(implicit readers:ReaderCache): Option[Bar] = for {
    obj <- ujson.read(rawJSON).objOpt
    foo <- Foo.fromVersionedJSON(obj("foo").render())
  } yield Bar(foo)
}

case class Bar(foo: Foo) extends WritesVersionedJSON[Bar] {
  override def toJSON(implicit versionIndex:VersionIndex): String = s"""{"foo":${foo.toVersionedJSON}}"""
}

/**
 * Wubba demonstrates a way to handle Versioned objects with nested uniform, homogeneous, collections of Versioned objects.
 */

object Wubba extends ReadsVersionedJSON[Wubba] {
  override val version: Version = 0.1

  override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()

  override def fromJSON(rawJSON: String)(implicit readers:ReaderCache): Option[Wubba] = for {
    obj <- ujson.read(rawJSON).objOpt
    barsObj <- obj("bars").objOpt
    barsArr <- ArrayJSON.fromJSON(barsObj.render())
  } yield Wubba(for (b <- barsArr) yield b.asInstanceOf[Bar])
}

case class Wubba(bars: Seq[Bar]) extends WritesVersionedJSON[Wubba] {
  override def toJSON(implicit versionIndex:VersionIndex): String = s"""{"bars":${ArrayJSON.toJSON(bars:_*)}}"""
}

/**
 * Wubba demonstrates a way to handle Versioned objects with nested, diverse, heterogeneous, collections of Versioned objects.
 */

object Woo extends ReadsVersionedJSON[Woo] {
  override val version: Version = 0.1
  override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()
  override def fromJSON(rawJSON: String)(implicit readers:ReaderCache): Option[Woo] = for {
    obj <- ujson.read(rawJSON).objOpt
    shapesObj <- obj("shapes").objOpt
    shapesArr <- ArrayJSON.fromJSON(shapesObj.render())
  } yield Woo(for (b <- shapesArr) yield b.asInstanceOf[Shape])
}

case class Woo(shapes: Seq[Shape]) extends WritesVersionedJSON[Woo] {
  override def toJSON(implicit versionIndex:VersionIndex): String = s"""{"shapes":${ArrayJSON.toJSON(shapes:_*)}}"""
}

object Point2D extends ReadsVersionedJSON[Point2D] {
  override val version: Version = 0.1
  override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()
  override def fromJSON(rawJSON: String)(implicit readers:ReaderCache): Option[Point2D] = for {
    obj <- ujson.read(rawJSON).objOpt
    arr <- obj("p").arrOpt
    x <- arr(0).numOpt
    y <- arr(1).numOpt
  } yield Point2D(x.toDouble, y.toDouble)
}

case class Point2D(x:Double = 0.0, y: Double = 0.0) extends WritesVersionedJSON[Point2D] {
  override def toJSON(implicit versionIndex: VersionIndex): String = s"""{"p":[$x,$y]}"""
}

object Color extends ReadsVersionedJSON[Color] {
  override val version: Version = 0.1
  override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()
  override def fromJSON(rawJSON: String)(implicit readers:ReaderCache): Option[Color] = for {
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
  override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()
  override def fromJSON(rawJSON: String)(implicit readers:ReaderCache): Option[Triangle] = {
    for {
      obj <- ujson.read(rawJSON).objOpt
      points <- obj("points").objOpt
      pointsArr <- ArrayJSON.fromJSON(points.render())
      position <- Point2D.fromVersionedJSON(obj("position").render())
      color <- Color.fromVersionedJSON(obj("color").render())
    } yield Triangle(pointsArr(0).asInstanceOf[Point2D], pointsArr(1).asInstanceOf[Point2D], pointsArr(2).asInstanceOf[Point2D], position, color)
  }
}

case class Triangle(p1: Point2D, p2: Point2D, p3: Point2D, override val position: Point2D = Point2D(), override val color:Color) extends Shape {
  override val points:Array[Point2D] = Array[Point2D](p1, p2, p3)
  override def toJSON(implicit versionIndex: VersionIndex): String = s"""{"points":${ArrayJSON.toJSON(p1, p2, p3)}, "position":${position.toVersionedJSON}, "color":${color.toVersionedJSON}}"""
}


object Rectangle extends ReadsVersionedJSON[Rectangle] {
  override val version: Version = 0.1
  override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()
  override def fromJSON(rawJSON: String)(implicit readers:ReaderCache): Option[Rectangle] = for {
    obj <- ujson.read(rawJSON).objOpt
    width <- obj("w").numOpt
    height <- obj("h").numOpt
    position <- Point2D.fromVersionedJSON(obj("position").render())
    color <- Color.fromVersionedJSON(obj("color").render())
  } yield Rectangle(width, height, position, color)
}

case class Rectangle(width: Double, height: Double, override val position: Point2D = Point2D(), override val color:Color) extends Shape {
  override val points:Array[Point2D] = Array[Point2D]( Point2D(width, height), Point2D(width, -height), Point2D(-width, -height), Point2D(-width, height) )
  override def toJSON(implicit versionIndex: VersionIndex): String = s"""{"w":$width,"h":$height,"position":${position.toVersionedJSON},"color":${color.toVersionedJSON}}"""
}

object Square extends ReadsVersionedJSON[Square] {
  override val version: Version = 0.1
  override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()
  override def fromJSON(rawJSON: String)(implicit readers:ReaderCache): Option[Square] = for {
    obj <- ujson.read(rawJSON).objOpt
    length <- obj("l").numOpt
    position <- Point2D.fromVersionedJSON(obj("position").render())
    color <- Color.fromVersionedJSON(obj("color").render())
  } yield Square(length, position, color)
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
  override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()
  override def fromJSON(rawJSON: String)(implicit readers:ReaderCache): Option[Circle] = for {
    obj <- ujson.read(rawJSON).objOpt
    radius <- obj("r").numOpt
    position <- Point2D.fromVersionedJSON(obj("position").render())
    color <- Color.fromVersionedJSON(obj("color").render())
  } yield Circle(radius, position, color)
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

/**
  Demonstrates Versioned classes with maps of versioned classes keyed by primitives.
 */

object MesopotamianRank extends ReadsVersionedJSON[MesopotamianRank] {
  override val version: Version = 0.1
  override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()

  override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[MesopotamianRank] = for {
    obj <- ujson.read(rawJSON).objOpt
    rank <- MapJSON.fromJSON(obj("rank").render())
  } yield {
    var r: immutable.TreeMap[Int, Mesopotamian] = immutable.TreeMap[Int, Mesopotamian]()
    for ((k, v) <- rank) {
      val k0 = k.asInstanceOf[Primitive.int]
      if (k0 != null) {
        val v0 = v.asInstanceOf[Mesopotamian]
        r = r + (k0.p -> v0)
      }
    }
    MesopotamianRank(r)
  }
}

case class MesopotamianRank(rank: immutable.TreeMap[Int, Mesopotamian]) extends WritesVersionedJSON[MesopotamianRank] {
  import Primitive._
  override def toJSON(implicit versionIndex: VersionIndex): String = s"""{"rank":${MapJSON.primativeKeyToJSON[Int, int, Mesopotamian](rank, int)}}"""
}

object Era extends ReadsVersionedJSON[Era] {
  override val version: Version = 0.1
  override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()

  override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[Era] = {
    import Primitive._
    for {
      obj <- ujson.read(rawJSON).objOpt
      born <- int.fromJSON(obj("born").render())
      died <- int.fromJSON(obj("died").render())
    } yield Era(born.p, died.p)
  }
}

case class Era(born: Int, died: Int) extends WritesVersionedJSON[Era] {
  override def toJSON(implicit versionIndex: VersionIndex): String = s"""{"born":$born,"died":$died}"""
}

object Mesopotamian extends ReadsVersionedJSON[Mesopotamian] {
  override val version: Version = 0.1
  override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()

  override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[Mesopotamian] = for {
    obj <- ujson.read(rawJSON).objOpt
    name <- obj("name").strOpt
    era <- Era.fromVersionedJSON(obj("era").render())
  } yield Mesopotamian(name, era)
}

case class Mesopotamian(name: String, era: Era) extends WritesVersionedJSON[Mesopotamian] {
  override def toJSON(implicit versionIndex: VersionIndex): String = s"""{"name":"$name","era":${era.toVersionedJSON}}"""
}

/**
  Demonstrates Versioned classes with maps of primitives keyed by versioned classes.
 */

object MesopotamianCitations extends ReadsVersionedJSON[MesopotamianCitations] {
  override val version: Version = 0.1
  override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()

  override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[MesopotamianCitations] = for {
    obj <- ujson.read(rawJSON).objOpt
    rank <- MapJSON.fromJSON(obj("citations").render())
  } yield {
    var c: immutable.Map[Mesopotamian, Int] = immutable.Map[Mesopotamian, Int]()
    for ((k, v) <- rank) {
      val k0 = k.asInstanceOf[Mesopotamian]
      if (k0 != null) {
        val v0 = v.asInstanceOf[Primitive.int]
        c = c + (k0 -> v0.p)
      }
    }
    MesopotamianCitations(c)
  }
}

case class MesopotamianCitations(citations: immutable.Map[Mesopotamian, Int]) extends WritesVersionedJSON[MesopotamianCitations] {
  import Primitive._
  override def toJSON(implicit versionIndex: VersionIndex): String = s"""{"citations":${MapJSON.primativeValueToJSON[Mesopotamian, Int, int](citations, int)}}"""
}


/**
 * Demonstrates Versioned classes with maps of versioned classes keyed by versioned classes.
 */

object TimeSquareObservations extends ReadsVersionedJSON[TimeSquareObservations] {
  override val version: Version = 0.1
  override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()

  override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[TimeSquareObservations] = for {
    obj <- ujson.read(rawJSON).objOpt
    observations <- MapJSON.fromJSON(obj("observations").render())
  } yield {
    var o: immutable.Map[Square, Era] = immutable.Map[Square, Era]()
    for ((k, v) <- observations) {
      val k0 = k.asInstanceOf[Square]
      if (k0 != null) {
        val v0 = v.asInstanceOf[Era]
        o = o + (k0 -> v0)
      }
    }
    TimeSquareObservations(o)
  }
}

case class TimeSquareObservations(observations: immutable.Map[Square, Era]) extends WritesVersionedJSON[TimeSquareObservations] {
  override def toJSON(implicit versionIndex: VersionIndex): String = s"""{"observations":${MapJSON.toJSON[Square, Era](observations)}}"""
}

object MapTests extends ReadsVersionedJSON[MapTests] {
  override val version: Version = 0.1
  override val oldVersions: Array[ReadsStaleJSON[_ <: Versioned]] = Array[ReadsStaleJSON[_ <: Versioned]]()

  import Primitive._
  override def fromJSON(rawJSON: String)(implicit readers: ReaderCache): Option[MapTests] = for {
    obj <- ujson.read(rawJSON).objOpt
    rankedLetters <- MapJSON.fromJSON(obj("rankedLetters").render())
    piePIE <- MapJSON.fromJSON(obj("piePIE").render())
  } yield {
    var rl: immutable.TreeMap[Int, Char] = immutable.TreeMap[Int, Char]()
    for ((k: int, v: char) <- rankedLetters) rl = rl + (k.p -> v.p)
    var pp: immutable.Map[Float, Double] = immutable.Map[Float, Double]()
    for ((k: float, v: double) <- piePIE) pp = pp + (k.p -> v.p)
    MapTests(rl, pp)
  }
}

case class MapTests(rankedLetters: immutable.TreeMap[Int, Char], piePIE: immutable.Map[Float, Double]) extends WritesVersionedJSON[TimeSquareObservations] {
  import Primitive._
  override def toJSON(implicit versionIndex: VersionIndex): String = s"""{"rankedLetters":${
      MapJSON.primativeKeyAndValueToJSON[Int, int, Char, char](rankedLetters, int, char)
    },"piePIE":${
      MapJSON.primativeKeyAndValueToJSON[Float, float, Double, double](piePIE, float, double)
    }}"""
}