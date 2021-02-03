package ai.dragonfly.versionedjson.examples

import ai.dragonfly.versionedjson.VersionedJSON

import scala.collection.immutable

object Test extends App {

  // Past versions of Foo serializations:
  val foo0_1json = """{"#v":["com.whatever.Foo:0.1"], "#o":[0,{"s": "foo", "f": 3.4028235E38, "i": 2147483647, "l": "9223372036854775807", "d": 1.7976931348623157E308}]}"""
  val foo0_2json = """{"#v":["com.whatever.Foo:0.2"], "#o":[0,{"s": "foo", "f": 3.4028235E38, "i": 2147483647, "b": true, "l": "9223372036854775807", "d": 1.7976931348623157E308}]}"""
  val foo0_3json = """{"#v":["ai.dragonfly.versionedjson.examples.Foo:0.3"], "#o":[0,{"f": 3.4028235E38, "i": 2147483647, "b": true, "l": "9223372036854775807", "d": 1.7976931348623157E308}]}"""

  // Versions of Bar
  val barjson =       s"""{"#v":["ai.dragonfly.versionedjson.examples.Bar:0.1","ai.dragonfly.versionedjson.examples.Foo:0.3"], "#o":[0,{"foo":[1,{"f": 3.4028235E38, "i": 2147483647, "b": true, "l": "9223372036854775807", "d": 1.7976931348623157E308}]}]}"""
  val barFoo0_1json = s"""{"#v":["ai.dragonfly.versionedjson.examples.Bar:0.1","com.whatever.Foo:0.1"], "#o":[0,{"foo":[1,{"s": "foo", "f": 3.4028235E38, "i": 2147483647, "l": "9223372036854775807", "d": 1.7976931348623157E308}]}]}"""
  val barFoo0_2json = s"""{"#v":["ai.dragonfly.versionedjson.examples.Bar:0.1","com.whatever.Foo:0.2"], "#o":[0,{"foo":[1,{"s": "foo", "f": 3.4028235E38, "i": 2147483647, "b": true, "l": "9223372036854775807", "d": 1.7976931348623157E308}]}]}"""

//  println(s"Registry blank: ${VersionedJSON.Readers}")
//  println(s"Initializing VersionedJsonReaderRegistry: ${VersionedJSON.Readers(Foo)}")

  val foo = new Foo(Integer.MAX_VALUE, Long.MaxValue, Float.MaxValue, Double.MaxValue, true)
  println(s"Initialized foo, the current version of ${Foo.version.cls}: $foo")

  val versionedJSON: String = foo.toVersionedJSON
  println(s"\n\nRead/Write Versioned JSON:")
  println(s"foo.toVersionedJSON => $versionedJSON")
  println(s"Foo.fromVersionedJSON($versionedJSON) => ${Foo.fromVersionedJSON(versionedJSON).orNull}")


  println("\nDeserialize stale Foo 0.1 JSON:")
  println(s"Foo0.1.json: ${foo0_1json}")
  val foo0_1: Foo$0_1 = VersionedJSON[Foo$0_1](foo0_1json).orNull
  println(s"\tval foo0_1 = VersionedJSON[Foo$$0_1](Foo0.1.json).orNull => $foo0_1")
  println("\tUpgrade Foo0.1 to Foo0.2:")
  println(s"\t\tfoo0_1.upgrade => ${foo0_1.upgrade}")
  println("\tRead Foo0.1.json and automatically upgrade to current version of Foo:")
  println(s"\t\tVersionedJSON[Foo](Foo0.1.json) => ${VersionedJSON[Foo](foo0_1json)}")
  println(s"Foo.fromVersionedJSON($foo0_1json) => ${Foo.fromVersionedJSON(foo0_1json).orNull}")

  println("\nDeserialize stale Foo 0.2 JSON:")
  println(s"Foo0.2.json: ${foo0_2json}")
  val foo0_2: Foo$0_2 = VersionedJSON[Foo$0_2](foo0_2json).orNull
  println(s"\tval foo0_2 = VersionedJSON.unwrap(Foo0.2.json)} => $foo0_2")
  println("\tUpgrade Foo0.2 to Foo0.3:")
  println(s"\t\tfoo0_2.upgrade => ${foo0_2.upgrade}")
  println("\tRead Foo0.2.json and automatically upgrade to current version of Foo:")
  println(s"\t\tVersionedJSON[Foo](Foo0.2.json) => ${VersionedJSON[Foo](foo0_2json)}")

  println(s"\n\nRead/Write Versioned Classes with option as fields:")
  val optionalFoo1: OptionalFoo = OptionalFoo(1, Some(foo))
  println(s"\tInitialized optionalFoo1, the current version of ${OptionalFoo.version.cls}: $optionalFoo1")

  val optionalFoo1VersionedJSON = optionalFoo1.toVersionedJSON
  println(s"\toptionalFoo1.toVersionedJSON => ${optionalFoo1VersionedJSON}")
  println(s"\tOptionalFoo.fromVersionedJSON(optionalFoo1.toVersionedJSON) => ${OptionalFoo.fromVersionedJSON(optionalFoo1VersionedJSON)}")

  val optionalFoo2: OptionalFoo = OptionalFoo(2, None)
  println(s"\tInitialized optionalFoo2, the current version of ${OptionalFoo.version.cls}: $optionalFoo2")

  val optionalFoo2VersionedJSON = optionalFoo2.toVersionedJSON
  println(s"\toptionalFoo2.toVersionedJSON => ${optionalFoo2VersionedJSON}")
  println(s"\tOptionalFoo.fromVersionedJSON(optionalFoo2.toVersionedJSON) => ${OptionalFoo.fromVersionedJSON(optionalFoo2VersionedJSON)}")


  println(s"\n\nRead/Write Nested Versioned Classes:")
  val bar: Bar = Bar(foo)
  println(s"\tInitialized bar, the current version of ${Bar.version.cls}: $bar")

  val barVersionedJSON = bar.toVersionedJSON
  println(s"\tbar.toVersionedJSON => ${barVersionedJSON}")
  val barFromVersionedJSON = Bar.fromVersionedJSON(barVersionedJSON).orNull
  println(s"\tBar.fromVersionedJSON(bar.toVersionedJSON) => ${barFromVersionedJSON}")
  println(s"\tbar == Bar.fromVersionedJSON(bar.toVersionedJSON)) => ${bar == barFromVersionedJSON}")
  if (bar != barFromVersionedJSON) throw new Exception("Serialization error!")

  println(s"\n\nRead/Write Bar with old versions of Foo:")
  println(s"\tCurrent Bar with Foo 0.1: $barFoo0_1json")
  println(s"\tBar.fromVersionedJSON(bar(foo0_1json)) => ${Bar.fromVersionedJSON(barFoo0_1json)}")
  println(s"\tCurrent Bar with Foo 0.2: $barFoo0_2json")
  println(s"\tBar.fromVersionedJSON(bar(foo0_2json)) => ${Bar.fromVersionedJSON(barFoo0_2json)}")
  println(s"\tCurrent Bar with Foo 0.3: $barjson")
  println(s"\tBar.fromVersionedJSON(bar(foo0_3json)) => ${Bar.fromVersionedJSON(barjson)}")

  // Wubba
  println(s"\n\nRead/Write Nested Versioned Classes with Nested Collections of Uniformly typed Versioned Classes:")
  val wubba: Wubba = Wubba(
    Seq(
      bar,
      Bar(Foo(3, 3L, 3.14159f, Math.PI, true)),
      Bar(Foo(42, 42L, 42.0f, 42.0, true)),
      Bar(Foo(69, 69L, 6.9f, 68.9999999, false))
    )
  )

  println(s"\tInitialized wubba, the current version of ${Wubba.version.cls}: $wubba")
  val wubbaVersionedJSON = wubba.toVersionedJSON
  println(s"\twubba.toVersionedJSON => ${wubbaVersionedJSON}")
  println(s"\tWubba.fromVersionedJSON(wubba.toJSON) => ${Wubba.fromVersionedJSON(wubbaVersionedJSON)}")

  // Woo
  println(s"\n\nRead/Write Nested Versioned Classes with Nested Collections of Diversely typed Versioned Classes:")
  val woo: Woo = Woo(
    Seq(
      Triangle(Point2D(0.0, 10.0), Point2D(-10.0, -5.0), Point2D(7.5, -3.0), Point2D(0.0, 0.0), Color(87, 2, 0)),
      Square(Math.PI, Point2D(0.0, 0.0), Color(87, 2, 0)),
      Rectangle(2*Math.PI, Math.E, Point2D(0.0, 0.0), Color(87, 2, 0)),
      Circle(Math.PI, Point2D(0.0, 0.0), Color(87, 2, 0))
    )
  )

  println(s"\tInitialized woo, the current version of ${Woo.version.cls}: $woo")
  val wooVersionedJSON = woo.toVersionedJSON
  println(s"\twoo.toVersionedJSON => ${wooVersionedJSON}")
  println(s"\tWoo.fromVersionedJSON(woo.toJSON) => ${Woo.fromVersionedJSON(wooVersionedJSON)}")

  println(s"\n\nRead/Write Maps of Versioned Classes keyed by Primitive Types:")
  val mesopotamianRank: MesopotamianRank = MesopotamianRank(
    immutable.TreeMap[Int, Mesopotamian](
      2 -> Mesopotamian("Sargon of Akkad", Era(-2334, -2279)),
      4 -> Mesopotamian("Ashurbanipal", Era(-668, -627)),
      1 -> Mesopotamian("Gilgamesh", Era(-2900, -2700)),
      3 -> Mesopotamian("Hammurabi", Era(-1792, -1750)),
    )
  )
  println(s"\tInitialized mesopotamianRank, the current version of ${MesopotamianRank.version.cls}: $mesopotamianRank")
  val mesopotamianRankJSON = mesopotamianRank.toVersionedJSON
  println(s"\tmesopotamianRank.toVersionedJSON => ${mesopotamianRankJSON}")
  println(s"\tMesopotamianRank.fromVersionedJSON(mesopotamianRank.toVersionedJSON) => ${MesopotamianRank.fromVersionedJSON(mesopotamianRankJSON)}")

  println(s"\n\nRead/Write Maps of Primitive Types keyed by Versioned Classes:")
  val mesopotamianCitations: MesopotamianCitations = MesopotamianCitations(
    immutable.Map[Mesopotamian, Int](
      Mesopotamian("Sargon of Akkad", Era(-2334, -2279)) -> 856956,
      Mesopotamian("Ashurbanipal", Era(-668, -627)) -> 354951,
      Mesopotamian("Gilgamesh", Era(-2900, -2700)) -> 988732627,
      Mesopotamian("Hammurabi", Era(-1792, -1750)) -> 2378,
    )
  )
  println(s"\tInitialized mesopotamianCitations, the current version of ${MesopotamianCitations.version.cls}: $mesopotamianCitations")
  val mesopotamianCitationsJSON = mesopotamianCitations.toVersionedJSON
  println(s"\tmesopotamianCitations.toVersionedJSON => ${mesopotamianCitationsJSON}")
  println(s"\tmesopotamianCitations.fromVersionedJSON(mesopotamianCitations.toVersionedJSON) => ${MesopotamianCitations.fromVersionedJSON(mesopotamianCitationsJSON)}")


  println(s"\n\nRead/Write Maps of Versioned Classes keyed by Versioned Classes:")
  val timeSquareObservations: TimeSquareObservations = TimeSquareObservations(
    immutable.Map[Square, Era](
      Square(Math.PI, Point2D(567.58, 0.0005), Color(255, 255, 0)) -> Era(1985, 1986),
      Square(Math.E, Point2D(2674.33589, 90.01119), Color(87, 2, 0)) -> Era(1955, 1956),
      Square(42.0, Point2D(0.0, 1.010063), Color(0, 255, 0)) -> Era(1885, 1886),
      Square(68.9999999, Point2D(11.0, 21.87), Color(128, 0, 128)) -> Era(2015, 2016)
    )
  )
  println(s"\tInitialized timeSquareObservations, the current version of ${TimeSquareObservations.version.cls}: $timeSquareObservations")
  val timeSquareObservationsJSON = timeSquareObservations.toVersionedJSON
  println(s"\ttimeSquareObservations.toVersionedJSON => ${timeSquareObservationsJSON}")
  println(s"\ttimeSquareObservations.fromVersionedJSON(timeSquareObservations.toVersionedJSON) => ${TimeSquareObservations.fromVersionedJSON(timeSquareObservationsJSON)}")


  println(s"\n\nRead/Write Maps of primitives keyed by primitives:")
  val primitiveMapTests = MapTests(
    immutable.TreeMap[Int, Char]( 1 -> 'A', 2 -> 'B', 3 -> 'C' ),
    immutable.Map[Float, Double]( Math.PI.toFloat -> Math.PI, Math.E.toFloat -> Math.E )
  )
  println(s"\tInitialized primitiveMapTests, the current version of ${MapTests.version.cls}: $primitiveMapTests")
  val primitiveMapTestsJSON = primitiveMapTests.toVersionedJSON
  println(s"\tprimitiveMapTests.toVersionedJSON => ${primitiveMapTestsJSON}")
  println(s"\tprimitiveMapTests.fromVersionedJSON(primitiveMapTests.toVersionedJSON) => ${MapTests.fromVersionedJSON(primitiveMapTestsJSON)}")

}
