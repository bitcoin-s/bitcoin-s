package simple

object Main {

  def main(args: Array[String]): Unit = {
    val result = Library.say("hello world")
    assert(result == 42)
  }

}
