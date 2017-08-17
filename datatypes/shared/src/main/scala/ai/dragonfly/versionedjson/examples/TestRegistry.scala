package ai.dragonfly.versionedjson.examples

import ai.dragonfly.versionedjson.VersionedJsonReaders

object TestRegistry {
  val registry = new VersionedJsonReaders()

  registry.registerVersionedJsonReader(
    "com.whatever.Foo" -> Foo,
    "ai.dragonfly.versionedjson.examples.Foo" -> Foo,
    "ai.dragonfly.versionedjson.examples.Bar" -> Bar
  )
}
