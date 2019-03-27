package ai.dragonfly.versionedjson.examples.test

//import ai.dragonfly.versionedjson.examples.{Bar, Foo, VersionsOfFooSerializations}
import ai.dragonfly.versionedjson.{Versioned, VersionedJson, VersionedJsonReaderRegistry, VersionedJsonReaders}

import scala.collection.immutable.HashSet

object Tests {

  val registry = TestRegistry
  println(VersionedJsonReaderRegistry)

  def testVersionedJson() = {

    val foo3 = Foo(Integer.MAX_VALUE, Long.MaxValue, Float.MaxValue, Double.MaxValue, true)
    println(s"$foo3 ${Foo.cls}")

    val jsonString: String = foo3.toJSON
    println(jsonString)

    val foo3b = Versioned.fromJSON[Foo](jsonString) match {
      case Some(foo: Foo) => foo;
      case o: Any => println(s"Error: $o")
    }

    println(foo3b)
    Versioned.fromJSON[Foo](jsonString) match {
      case Some(foo: Foo) => foo;
      case o: Any => println(s"Error: $o")
    }
    println(foo3b)

    println("Foo Version 0.1: ")
    val foo1 = VersionedJson.fromJSON[Foo$0_1](VersionsOfFooSerializations.v0_1)
    println(foo1)

    println("Foo Version 0.2: ")
    val foo2 = VersionedJson.fromJSON[Foo$0_2](VersionsOfFooSerializations.v0_2)
    println(foo2)

    val f1_2: Option[(Foo, Foo)] = for {
      f1 <- foo1
      f2 <- foo2
      f1_3 <- VersionedJson.upgrade[Foo](f1)
      f2_3 <- VersionedJson.upgrade[Foo](f1)
    } yield {
      (f1_3, f2_3)
    }
    println(f1_2)

    println("Foo Version 0.1 -> Foo Version 0.3: ")
    val foo4 = VersionedJson.fromJSON[Foo](VersionsOfFooSerializations.v0_1)
    println(foo4)

    println("Foo Version 0.2 -> Foo Version 0.3: ")
    val foo5 = VersionedJson.fromJSON[Foo](VersionsOfFooSerializations.v0_2)
    println(foo5)

    println("Foo Version 0.1 -> Foo Version 0.2: ")
    val foo6 = VersionedJson.fromJSON[Foo$0_2](VersionsOfFooSerializations.v0_2)
    println(foo6)

  }

}
