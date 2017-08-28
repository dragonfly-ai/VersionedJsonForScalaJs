package ai.dragonfly.versionedjson.examples.test

import ai.dragonfly.versionedjson.examples.{Bar, Foo, TestRegistry, VersionsOfFooSerializations}
import ai.dragonfly.versionedjson.{VersionedJson, VersionedJsonReaders}

import scala.collection.immutable.HashSet

object Tests {

  def testVersionedJson() = {

    implicit val registry: VersionedJsonReaders = TestRegistry.registry

    val foo3 = Foo(Integer.MAX_VALUE, Long.MaxValue, Float.MaxValue, Double.MaxValue, true)
    val jsonString: String = foo3.JSON
    println(jsonString)
    val foo3b = VersionedJson.fromJson[Foo](jsonString) match {
      case Some(foo: Foo) => foo;
      case _ => null
    }
    println(foo3b)
    println("Foo Version 0.1: ")
    val foo1 = VersionedJson.fromJson[Foo](VersionsOfFooSerializations.v0_1)
    println(foo1)
    println("Foo Version 0.2: ")
    val foo2 = VersionedJson.fromJson[Foo](VersionsOfFooSerializations.v0_2)
    println(foo2)

    for {
      f1 <- foo1
      f2 <- foo2
    } yield HashSet(f1.asInstanceOf[Foo], f2.asInstanceOf[Foo], foo3) match {
      case fooSet: Set[Foo] =>
        val bar1 = Bar("two guys walked into a bar", fooSet)
        println(bar1)
        val bar1Json = bar1.JSON
        println(bar1Json)
        println(VersionedJson.fromJson[Bar](bar1Json))
      case _ => println("could not create instance of Bar")
    }
  }
}
