package ai.dragonfly.versionedjson.native

import ai.dragonfly.versionedjson.{ReadsStaleJSON, ReadsVersionedJSON, Version}

import scala.reflect.runtime.universe

/**
 * Trait for classes and companion objects
 */

trait Versioned

object ClassTag {
  //this.getClass.getName.split("\\$")(0))
  def apply[T](className: String): scala.reflect.ClassTag[T] = scala.reflect.ClassTag[T](java.lang.Class.forName(className))
}

object InferVersionFromReader {
  /**
   * Use Reflection on the JVM.
   * @param v an example of the versioned class.
   * @tparam T The type of the versioned class.
   * @return the version info for this class, taken from its reading object
   */
  def apply[T <: ai.dragonfly.versionedjson.Versioned](v: Versioned): Version = {

    val runtimeMirror = universe.runtimeMirror(v.getClass.getClassLoader)
    val module = runtimeMirror.staticModule(s"${v.getClass.getName}")
    val obj = runtimeMirror.reflectModule(module)

    obj.instance match {
      case rvj: ReadsVersionedJSON[T] => rvj.version
      case rsj: ReadsStaleJSON[T] => rsj.version
    }
  }
}
