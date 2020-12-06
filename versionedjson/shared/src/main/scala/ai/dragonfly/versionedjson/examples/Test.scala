package ai.dragonfly.versionedjson.examples

import ai.dragonfly.versionedjson.VersionedJSON

object Test extends App {

  // Past versions of Foo serializations:
  val v0_1 = """{ "#cls": "com.whatever.Foo", "#vid": 0.1, "#obj": {"s": "foo", "f": 3.4028235E38, "i": 2147483647, "l": "9223372036854775807", "d": 1.7976931348623157E308}}"""
  val v0_2 = """{"#obj": {"s": "foo", "f": 3.4028235E38, "i": 2147483647, "b": true, "l": "9223372036854775807", "d": 1.7976931348623157E308}, "#cls": "com.whatever.Foo", "#vid": 0.2}"""
  val v0_3 = """{"#obj": {"f": 3.4028235E38, "i": 2147483647, "b": true, "l": "9223372036854775807", "d": 1.7976931348623157E308}, "#cls": "ai.dragonfly.versionedjson.examples.Foo", "#vid": 0.3}"""

//  println(s"Registry blank: ${VersionedJSON.Readers}")
//  println(s"Initializing VersionedJsonReaderRegistry: ${VersionedJSON.Readers(Foo)}")

  val foo3 = Foo(Integer.MAX_VALUE, Long.MaxValue, Float.MaxValue, Double.MaxValue, true)
  println(s"Initialized foo3, the current version of ${Foo.cls}: $foo3")

  println(s"\n\nRead/Write plain JSON:")
  val json: String = foo3.toJSON
  println(s"foo3.toJSON => $json")
  println(s"Foo.fromJSON($json) => ${Foo.fromJSON(json)}")

  val versionedJSON: String = foo3.toVersionedJSON
  println(s"\n\nRead/Write Versioned JSON:")
  println(s"foo3.toVersionedJSON => $versionedJSON")
  println(s"Foo.fromVersionedJSON($versionedJSON) => ${Foo.fromVersionedJSON(versionedJSON).orNull}")

  println("\nDeserialize stale Foo 0.1 JSON:")
  println(s"Foo0.1.json: ${v0_1}")
  val foo0_1: Foo$0_1 = VersionedJSON.unwrap(v0_1).orNull.asInstanceOf[Foo$0_1]
  println(s"\tval foo0_1 = VersionedJSON.unwrap(Foo0.1.json)} => $foo0_1")
  println("\tUpgrade Foo0.1 to Foo0.2:")
  println(s"\t\tfoo0_1.upgrade => ${foo0_1.upgrade}")
  println("\tRead Foo0.1.json and automatically upgrade to current version of Foo:")
  println(s"\t\tVersionedJSON[Foo](Foo0.1.json) => ${VersionedJSON[Foo](v0_1)}")

  println("\nDeserialize stale Foo 0.2 JSON:")
  println(s"Foo0.2.json: ${v0_2}")
  val foo0_2: Foo$0_2 = VersionedJSON.unwrap(v0_2).orNull.asInstanceOf[Foo$0_2]
  println(s"\tval foo0_2 = VersionedJSON.unwrap(Foo0.2.json)} => $foo0_2")
  println("\tUpgrade Foo0.2 to Foo0.3:")
  println(s"\t\tfoo0_2.upgrade => ${foo0_2.upgrade}")
  println("\tRead Foo0.2.json and automatically upgrade to current version of Foo:")
  println(s"\t\tVersionedJSON[Foo](Foo0.2.json) => ${VersionedJSON[Foo](v0_2)}")

}
