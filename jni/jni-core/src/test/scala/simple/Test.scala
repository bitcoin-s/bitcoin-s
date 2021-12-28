package simple

import org.scalatest.flatspec._

class SimpleSpec extends AnyFlatSpec {

  "Calling native methods in tests" should "work" in {
    assert(Library.say("hello") == 42)
  }

}
