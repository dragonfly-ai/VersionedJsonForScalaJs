# versionedjson
This experiment aims to provide Scala and Scala.js with a common library for reading and writing compact versioned JSON objects.&nbsp;&nbsp;Past efforts at versioned serialization uncovered seemingly deal breaking axioms such as: "Versioned serialization schemes cannot guarantee compatibility and allow field deletion."&nbsp;&nbsp;These constraints led entire communities to the conclusion that any systems designed around versioned serialization would inevitably suffer from bloated classes with redundant data members and monotonically increasing levels of nausea for anyone reading the code.

Fortunately, a subtle assumption about design goals underlies the way architects and developers react to these axioms.&nbsp;&nbsp;This library demonstrates a few principles for disciplined versioned object serialization that sacrifice mathematical guaranties for practical utility.&nbsp;&nbsp;Most notably, by sacrificing automated serialization methods, like those based on reflection or macros, and embracing the limitation of forward only compatibility, versionedjson shifts system design pain points to hand written serialization and upgrade methods from coordinating many data structures and conversion between them, e.g. the relational database table for persistence, the server side class definition for handling client requests, the client side object representation usually in some form of JavaScript, and the various serialization formats required to transfer objects between application layers.

To use this library with SBT:
<pre>
resolvers += "dragonfly.ai" at "https://code.dragonfly.ai/"
libraryDependencies += "ai.dragonfly.code" %% "versionedjson" % "0.203"
</pre><br />

As of this release, versionedjson can compactly serialize:
<ul>
<li>Classes named T that extend trait: WritesVersionedJSON[T] with companion objects that extend trait: ReadsVersionedJSON[T]</li>
<li>Classes with fields of primitive types: Int, Short, Long, Float, Char, String, Double, Byte, Boolean.</li>
<li>Classes with fields of type: T that extend trait: WritesVersionedJSON[T].</li>
<li>Classes with fields of type: Option[WritesVersionedJSON[T]] or Options of primitives.</li>
<li>Classes with collections of primitives.
<li>Classes with collections of Option[WritesVersionedJSON[T]] or Options of primitives.</li>
<li>Classes with collections of objects of type: T that extend trait: WritesVersionedJSON[T].</li>
<li>Classes with maps with keys and values of types: primitive, WritesVersionedJSON[T], Option[WritesVersionedJSON[T]], and options of primitives.</li>
</ul>

versionedjson can deserialize:
<ul>
<li>Classes with fields of primitive types: Int, Short, Long, Float, Char, String, Double, Byte, Boolean.</li>
<li>Classes named T that extend trait: WritesVersionedJSON[T] with companion objects that extend trait: ReadsVersionedJSON[T]</li>
<li>Classes named T that extend trait: OldVersionOf[T &lt;: Versioned] with companion objects that extend trait: ReadsStaleJSON[T]</li>
<li>Classes with fields, options, collections, or maps of OldVersionOf[T &lt;: Versioned] or WritesVersionedJSON[T].</li>
</ul>

By design, this library discourages any serialization of any old versions of current classes.  If you need to modify or write an old version, simply upgrade it to the current version first, then write that version.

Upgrade methods, like the serialization and deserialization methods, must be hand written, but only have to upgrade from the previous version to the current version.  As the data model evolves, each version of a class becomes a link in a version chain which is automatically followed from version n to the current version.  This minimizes the need for developers to consider past versions of their classes as their codebase evolves.

This convention assumes that developers track versions of their codebases with source control.  By viewing past commits, they can resolve errors in version changes and upgrade methods.  Likewise, it assumes an immutable persistence model which never mutates or deletes old versions of classes so that developers can restore information from past versions to resolve errors related to version upgrades or to recover deleted fields if they become useful in the future.

Enormous thanks to <a href="https://www.lihaoyi.com/">Li Haoyi</a> and the <a href="https://github.com/lihaoyi/upickle/tree/master/ujson">ujson</a> json serialization library which provides core functionality.