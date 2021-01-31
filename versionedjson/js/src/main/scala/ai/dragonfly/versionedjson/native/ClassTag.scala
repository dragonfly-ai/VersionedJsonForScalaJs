package ai.dragonfly.versionedjson.native

import ai.dragonfly.versionedjson
import versionedjson._

import scala.collection.mutable
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
      case _ =>
        val version: Version = Version.parseVersionString(className)
        throw UnknownVersion(version)
    }
  }
}

object LoadReader {
  val knownReaders: mutable.HashMap[String, ReadsJSON[_]] = mutable.HashMap[String, ReadsJSON[_]]()

  /**
   * Use lookupLoadableModuleClass in JS environments.
   * @param v an example of the versioned class.
   * @tparam T The type of the versioned class.
   * @return the version info for this class, taken from its reading object
   */

  def apply[T <: ai.dragonfly.versionedjson.Versioned](v: ai.dragonfly.versionedjson.Versioned): ReadsJSON[T] = {
    val companionObjectName: String = s"${v.getClass.getName}$$"
    knownReaders.get(companionObjectName).orElse[ReadsJSON[_]](Some(
      Reflect.lookupLoadableModuleClass(companionObjectName) match {
        case Some(lmc: LoadableModuleClass) =>
          lmc.loadModule() match {
            case rvj: ReadsVersionedJSON[_] =>
              VersionedJSON.Readers(rvj)
              knownReaders.put(companionObjectName, rvj)
              rvj
            case rsj: ReadsStaleJSON[_] =>
              knownReaders.put(companionObjectName, rsj)
              rsj
          }
        case _ => throw UnknownReader(v)
      }
    )).get.asInstanceOf[ReadsJSON[T]]
  }
}