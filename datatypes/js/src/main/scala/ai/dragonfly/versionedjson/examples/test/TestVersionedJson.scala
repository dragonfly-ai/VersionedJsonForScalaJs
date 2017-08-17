package ai.dragonfly.versionedjson.examples.test

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("TestVersionedJson")
object TestVersionedJson {

  @JSExport
  def test(): Unit = Tests.testVersionedJson()
}
