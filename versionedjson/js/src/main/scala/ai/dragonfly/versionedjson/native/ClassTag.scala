package ai.dragonfly.versionedjson.native

import ai.dragonfly.versionedjson.{ReadsStaleJSON, ReadsVersionedJSON, UnknownVersionedClass, Version}

import scala.scalajs.reflect.annotation.EnableReflectiveInstantiation
import scala.scalajs.reflect.{InstantiatableClass, LoadableModuleClass, Reflect}

/**
 * Trait for classes and companion objects
 */

@EnableReflectiveInstantiation
trait Versioned

object ClassTag {
  def apply[T <: Versioned](className:String): scala.reflect.ClassTag[T] = {
    Reflect.lookupInstantiatableClass(className) match {
      case Some(ic: InstantiatableClass) => scala.reflect.ClassTag[T](ic.runtimeClass)
      case _ => throw UnknownVersionedClass(s"Unknown Versioned Classname: $className")
    }
  }
}

object InferVersionFromReader {
  /**
   * Use lookupLoadableModuleClass in JS environments.
   * @param v an example of the versioned class.
   * @tparam T The type of the versioned class.
   * @return the version info for this class, taken from its reading object
   */

  def apply[T <: ai.dragonfly.versionedjson.Versioned](v: Versioned): Version = {
    val companionObjectName: String = s"${v.getClass.getName}$$"
    Reflect.lookupLoadableModuleClass(companionObjectName) match {
      case Some(lmc: LoadableModuleClass) =>
        lmc.loadModule() match {
          case rvj: ReadsVersionedJSON[T] => rvj.version
          case rsj: ReadsStaleJSON[T] => rsj.version
        }
      case _ => throw UnknownVersionedClass(s"Unknown Versioned Companion Object: $companionObjectName")
    }
  }
}