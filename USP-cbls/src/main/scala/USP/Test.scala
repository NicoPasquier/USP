package USP

import java.io._
import scala.io.Source
import USP.CBLS._
object Test {
  def main(args: Array[String]): Unit = {
    val parser = new ConverterJson(args(0))
    //val parser = new ConverterJson("/home/etud/USP/timetabling-dev/src/choco/Démo/tmp/experiment_22-05-23/solution_ua_l1-l2_p1-p6_l3info_m1info-s1_m2info-s1-s2_v1_extension_v2_220523_01_04_35_extension_v2.json")
    //val parser = new ConverterJson("/home/etud/USP/USP/instance/test_extension_v2.json")
    val inst = parser.readJson()

    val x_slot = CBLS(inst)

    write_solution_file(inst, x_slot, args(1))
  }
}
