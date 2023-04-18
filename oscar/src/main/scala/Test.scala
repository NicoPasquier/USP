

object Test {
  def main(args: Array[String]): Unit = {
    val parser = new ConverterJson(args(0))
    val  inst = parser.readJson()
    println(inst)
  }
}
