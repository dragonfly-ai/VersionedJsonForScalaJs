package ai.dragonfly.versionedjson.examples

import ai.dragonfly.versionedjson.VersionedJSON

object Test extends App {

  // Past versions of Foo serializations:
  val foo0_1json = """{ "#cls": "com.whatever.Foo", "#vid": 0.1, "#obj": {"s": "foo", "f": 3.4028235E38, "i": 2147483647, "l": "9223372036854775807", "d": 1.7976931348623157E308}}"""
  val foo0_2json = """{"#obj": {"s": "foo", "f": 3.4028235E38, "i": 2147483647, "b": true, "l": "9223372036854775807", "d": 1.7976931348623157E308}, "#cls": "com.whatever.Foo", "#vid": 0.2}"""
  val foo0_3json = """{"#obj": {"f": 3.4028235E38, "i": 2147483647, "b": true, "l": "9223372036854775807", "d": 1.7976931348623157E308}, "#cls": "ai.dragonfly.versionedjson.examples.Foo", "#vid": 0.3}"""

  // Versions of Bar
  val barjson = s"""{"#vid":0.1,"#cls":"ai.dragonfly.versionedjson.examples.Bar","#obj":{"foo":$foo0_3json}}"""
  val barFoo0_1json = s"""{"#vid":0.1,"#cls":"ai.dragonfly.versionedjson.examples.Bar","#obj":{"foo":$foo0_1json}}"""
  val barFoo0_2json = s"""{"#vid":0.1,"#cls":"ai.dragonfly.versionedjson.examples.Bar","#obj":{"foo":$foo0_2json}}"""

//  println(s"Registry blank: ${VersionedJSON.Readers}")
//  println(s"Initializing VersionedJsonReaderRegistry: ${VersionedJSON.Readers(Foo)}")

  val foo = new Foo(Integer.MAX_VALUE, Long.MaxValue, Float.MaxValue, Double.MaxValue, true)
  println(s"Initialized foo, the current version of ${Foo.cls}: $foo")

  println(s"\n\nRead/Write plain JSON:")
  val json: String = foo.toJSON
  println(s"foo.toJSON => $json")
  println(s"Foo.fromJSON($json) => ${Foo.fromJSON(json)}")

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
  println(s"\tInitialized bar, the current version of ${Bar.cls}: $bar")
  val barJSON = bar.toJSON
  println(s"\tbar.toJSON => $barJSON")
  println(s"\tBar.fromJSON(bar.toJSON) => ${Bar.fromJSON(barJSON)}")
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

}
