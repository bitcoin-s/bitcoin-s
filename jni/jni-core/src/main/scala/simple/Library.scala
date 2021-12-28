package simple

import com.github.sbt.jni.nativeLoader

@nativeLoader("demo0")
object Library {

  @native def say(message: String): Int

}
