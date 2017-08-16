package ai.dragonfly.types

import microjson._

import scala.collection.immutable.HashMap

object MicroJsonTest extends App {

  val s = Json.write(
    new JsObject(
      HashMap[String, JsValue](
        ("asdf", new JsNumber("10")),
        ("arr", new JsArray(Seq(JsString("10"), JsString("10")))),
        ("fdsa", new JsNumber("-1"))
      ).asInstanceOf[Map[String, JsValue]]
    )
  )

  println(s)

  val parsed = Json.read(s)

  println(parsed.value.asInstanceOf[Map[String, JsValue]])
  println(parsed)
}