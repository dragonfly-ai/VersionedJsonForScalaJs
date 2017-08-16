package ai.dragonfly.types

object TestRegistry {
  val registry = new VersionedJSONReaders()

  registry.registerVersionedJsonReader(
    "com.whatever.Foo" -> Foo,
    "ai.dragonfly.types.Foo" -> Foo,
    "ai.dragonfly.types.Bar" -> Bar
  )
}
