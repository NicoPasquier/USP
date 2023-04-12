

object Test {
  def main(args: Array[String]): Unit = {
    val parser = new ConverterJson("solution_ua_m2-informatique-s1-s2_extension_v2_040423_11_16_26_extension_v2.json")
    val  inst = parser.readJson()
    println(inst)
  }
}
