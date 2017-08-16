package ai.dragonfly

import microjson.JsValue

import scala.collection.immutable.Map

package object types {
  type JsObj = Map[String, JsValue]
  def JsObj(elems: (String, JsValue)*): JsObj = elems.toMap[String, JsValue]
}