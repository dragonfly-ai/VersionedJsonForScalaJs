package ai.dragonfly.versionedjson.examples.test

object VersionsOfFooSerializations {
  val v0_1 = """{ "#cls": "com.whatever.Foo", "#vid": 0.1, "#obj": {"s": "foo", "f": 3.4028235E38, "i": 2147483647, "l": "9223372036854775807", "d": 1.7976931348623157E308}}"""
  val v0_2 = """{"#obj": {"s": "foo", "f": 3.4028235E38, "i": 2147483647, "b": true, "l": "9223372036854775807", "d": 1.7976931348623157E308}, "#cls": "com.whatever.Foo", "#vid": 0.2}"""
  val v0_3 = """{"#obj": {"f": 3.4028235E38, "i": 2147483647, "b": true, "l": "9223372036854775807", "d": 1.7976931348623157E308}, "#cls": "ai.dragonfly.versionedjson.examples.Foo", "#vid": 0.3}"""
}
