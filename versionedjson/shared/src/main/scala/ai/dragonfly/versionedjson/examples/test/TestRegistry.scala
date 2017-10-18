package ai.dragonfly.versionedjson.examples.test

import ai.dragonfly.versionedjson.VersionedJsonReaders

object TestRegistry {
  val registry = new VersionedJsonReaders()

  registry.registerVersionedJsonReader(
    Foo
  )
}
