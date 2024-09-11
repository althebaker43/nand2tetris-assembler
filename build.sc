import mill._, scalalib._
import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`

object assembler extends RootModule with ScalaModule {
  def scalaVersion = "3.4.2"
  object test extends ScalaTests with TestModule.ScalaTest {
    override def ivyDeps: T[Agg[Dep]] = Agg(ivy"org.scalatest::scalatest:3.2.19")
  }
}

//
// Note the use of `millSourcePath`, `T.dest`, and `PathRef` when preforming
// various filesystem operations:
//
// 1. `millSourcePath` refers to the base path of the module. For the root
//    module, this is the root of the repo, and for inner modules it would be
//    the module path e.g. for module `foo.bar.qux` the `millSourcePath` would
//    be `foo/bar/qux`. This can also be overriden if necessary
//
// 2. `T.dest` refers to the destination folder for a task in the `out/`
//    folder. This is unique to each task, and can act as both a scratch space
//    for temporary computations as well as a place to put "output" files,
//    without worrying about filesystem conflicts with other tasks
//
// 3. `PathRef` is a way to return the *contents* of a file or folder, rather
//    than just its path as a string. This ensures that downstream tasks
//    properly invalidate when the contents changes even when the path stays
//    the same

/** Usage

> mill run
Foo2.value: <h1>hello2</h1>
Foo.value: <h1>hello</h1>
FooA.value: hello A
FooB.value: hello B
FooC.value: hello C
MyResource: My Resource Contents
MyOtherResource: My Other Resource Contents
my.custom.property: my-prop-value
MY_CUSTOM_ENV: my-env-value

> mill show assembly
".../out/assembly.dest/out.jar"

> ./out/assembly.dest/out.jar # mac/linux
Foo2.value: <h1>hello2</h1>
Foo.value: <h1>hello</h1>
FooA.value: hello A
FooB.value: hello B
FooC.value: hello C
MyResource: My Resource Contents
MyOtherResource: My Other Resource Contents
my.custom.property: my-prop-value

*/

//// SNIPPET:FATAL_WARNINGS

/** Usage

> sed -i 's/Foo2 {/Foo2 { println(this + "hello")/g' custom-src/Foo2.scala

> mill compile # demonstrate -deprecation/-Xfatal-warnings flags
error: object Foo2 { println(this + "hello")
error:                       ^
error: ...Implicit injection of + is deprecated. Convert to String to call +...

*/
