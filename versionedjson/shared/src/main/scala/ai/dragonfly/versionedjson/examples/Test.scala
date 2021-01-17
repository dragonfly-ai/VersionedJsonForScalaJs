package ai.dragonfly.versionedjson.examples

import ai.dragonfly.versionedjson.VersionedJSON

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
  val foo0_1: Foo$0_1 = VersionedJSON.unwrap(foo0_1json).orNull.asInstanceOf[Foo$0_1]
  println(s"\tval foo0_1 = VersionedJSON.unwrap(Foo0.1.json)} => $foo0_1")
  println("\tUpgrade Foo0.1 to Foo0.2:")
  println(s"\t\tfoo0_1.upgrade => ${foo0_1.upgrade}")
  println("\tRead Foo0.1.json and automatically upgrade to current version of Foo:")
  println(s"\t\tVersionedJSON[Foo](Foo0.1.json) => ${VersionedJSON[Foo](foo0_1json)}")
  println(s"Foo.fromVersionedJSON($foo0_1json) => ${Foo.fromVersionedJSON(foo0_1json).orNull}")

  println("\nDeserialize stale Foo 0.2 JSON:")
  println(s"Foo0.2.json: ${foo0_2json}")
  val foo0_2: Foo$0_2 = VersionedJSON.unwrap(foo0_2json).orNull.asInstanceOf[Foo$0_2]
  println(s"\tval foo0_2 = VersionedJSON.unwrap(Foo0.2.json)} => $foo0_2")
  println("\tUpgrade Foo0.2 to Foo0.3:")
  println(s"\t\tfoo0_2.upgrade => ${foo0_2.upgrade}")
  println("\tRead Foo0.2.json and automatically upgrade to current version of Foo:")
  println(s"\t\tVersionedJSON[Foo](Foo0.2.json) => ${VersionedJSON[Foo](foo0_2json)}")

  println(s"\n\nRead/Write Nested Versioned Classes:")
  val bar: Bar = Bar(foo)
  println(s"\tInitialized bar, the current version of ${Bar.version.cls}: $bar")

  val barVersionedJSON = bar.toVersionedJSON
  println(s"\tbar.toVersionedJSON => ${barVersionedJSON}")
  println(s"\tBar.fromVersionedJSON(bar.toJSON) => ${Bar.fromVersionedJSON(barVersionedJSON)}")

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
      Bar(Foo(3, System.currentTimeMillis(), 3.14159f, Math.PI, true)),
      Bar(Foo(42, 424242424L, 42.0f, 42.0, false)),
      Bar(Foo(69, 69696969L, 6.9f, 68.9999999, true))
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
      Triangle(new Point2D(0.0, 10.0), new Point2D(-10.0, -5.0), new Point2D(7.5, -3.0), new Point2D(0.0, 0.0), new Color(87, 2, 0)),
      Square(Math.PI, new Point2D(0.0, 0.0), new Color(87, 2, 0)),
      Rectangle(2*Math.PI, Math.E, new Point2D(0.0, 0.0), new Color(87, 2, 0)),
      Circle(Math.PI, new Point2D(0.0, 0.0), new Color(87, 2, 0))
    )
  )
  println(s"\tInitialized woo, the current version of ${Woo.version.cls}: $woo")
  val wooVersionedJSON = woo.toVersionedJSON
  println(s"\twoo.toVersionedJSON => ${wooVersionedJSON}")
  println(s"\tWoo.fromVersionedJSON(woo.toJSON) => ${Woo.fromVersionedJSON(wooVersionedJSON)}")

}
