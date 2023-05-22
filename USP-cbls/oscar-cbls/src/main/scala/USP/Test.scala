import java.io._
import scala.io.Source

package USP {

  object Test {
    def main(args: Array[String]): Unit = {
      //val parser = new ConverterJson(args(0))
      val parser = new ConverterJson("/home/etud/USP/timetabling-dev/src/choco/Démo/tmp/experiment_22-05-23/solution_ua_l1-l2_p1-p6_l3info_m1info-s1_m2info-s1-s2_v1_extension_v2_220523_01_04_35_extension_v2.json")
      //val parser = new ConverterJson("/home/etud/USP/USP/oscar/instance/test_extension_v2.json")
      val inst = parser.readJson()

      CBLS.CBLS(inst)

      write_solution_file(inst)
    }

    def write_solution_file(instance: InstanceUTP): Unit = {
      var out: String = ""

      val lines = Source.fromFile("/home/etud/USP/USP/oscar/instance/test.xml").getLines().toList

      var i: Int = 0

      while (lines(i).trim() != "<sessions>" && i < lines.length - 1) {
        out += lines(i) + "\n"
        i += 1
      }

      out += print_xml(instance)
      out += "\n</solution>\n</timetabling>"

      val file = new File("/home/etud/USP/USP/oscar/instance/test_v.xml")
      val bw = new BufferedWriter(new FileWriter(file))

      bw.write(out)

      bw.close()
    }

    def print_xml(instance: InstanceUTP): String = {
      var out: String = "<classes>\n"

      for (i <- 0 until instance.DATA.nr_classes) {
        out += "<class refId = \"" + instance.DATA.class_name(i) + "\">\n"
        out += "    <groups>\n" + print_xml_group_solution(instance, instance.SOLUTION.SESSIONS.session_class(i) - 1) + "    </groups>\n"
        out += "    <teachers>\n" + print_xml_teacher_solution(instance, instance.SOLUTION.SESSIONS.session_class(i) - 1) + "    </teachers>\n"
        out += "    <rooms>\n" + print_xml_room_solution(instance, instance.SOLUTION.SESSIONS.session_class(i) - 1) + "    </rooms>\n"
        out += "</class>\n"
      }

      out += "</classes>\n<sessions>\n"

      for (i <- 0 until instance.DATA.nr_sessions) {
        out += "<session rank= \"" + instance.SOLUTION.SESSIONS.session_rank(i) + "\" class=\"" + instance.DATA.class_name(instance.SOLUTION.SESSIONS.session_class(i) - 1) + "\">\n"
        out += "    <startingSlot dailySlot=\"" + instance.SOLUTION.SESSIONS.session_dailyslot(i) + "\" day=\"" + instance.SOLUTION.SESSIONS.session_day(i) + "\" week=\"" + instance.SOLUTION.SESSIONS.session_week(i) + "\" />\n"
        out += "    <rooms>\n" + print_xml_room_solution(instance, i) + "    </rooms>\n"
        out += "    <teachers>\n" + print_xml_teacher_solution(instance, i) + "    </teachers>\n"
        out += "</session>\n"
      }

      out += "</sessions>\n"

      return out
    }

    def print_xml_group_solution(instance: InstanceUTP, session: Int): String = {
      var out: String = ""
      val groups = instance.SOLUTION.CLASS.class_groups(instance.SOLUTION.SESSIONS.session_class(session) - 1)(1).set
      for (g <- 0 until groups.length) {
        out += "    <group refId=\"" + instance.SOLUTION.GROUPS.group_name(groups(g) - 1) + "\"/>\n"
      }

      return out
    }

    def print_xml_teacher_solution(instance: InstanceUTP, session: Int): String = {
      var out: String = ""

      for (t <- instance.SOLUTION.SESSIONS.session_teachers(session).set) {
        out += "        <teacher refId=\"" + instance.DATA.teacher_name(t - 1) + "\"/>\n"
      }

      return out
    }

    def print_xml_room_solution(instance: InstanceUTP, session: Int): String = {
      var out: String = ""

      for (r <- instance.SOLUTION.SESSIONS.session_rooms(session).set) {
        out += "        <room refId=\"" + instance.DATA.room_name(r - 1) + "\"/>\n"
      }

      return out
    }

  }

}