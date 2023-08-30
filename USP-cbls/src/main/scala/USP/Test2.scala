package USP

import USP.CBLS._
import oscar.cbls._

import java.io.{BufferedWriter, File, FileWriter}

object Test2 {
  def main(args: Array[String]): Unit = {

//    val parser = new ConverterJson(args(0))
    val parser = new ConverterJson("/home/etud/USP/USP/instance/m2_NP/solution_ua_m2-informatique-s1-s2-NP_extension_v2_160823_22_32_32_extension_v2.json")
    val inst = parser.readJson()

    var solution_trouve = 0
    var solution_non_trouve = 0

    var out: String = ""

    for(semaine <- 1 to inst.DATA.nr_weeks){
      for(jour <- 1 to inst.DATA.nr_days_per_week){
//        println("journée " + ((semaine - 1) * inst.DATA.nr_days_per_week + jour) + "; semaine: " + semaine + "; jour: " + jour)
        out += ((semaine - 1) * inst.DATA.nr_days_per_week + jour) + ";" + semaine + ";" + jour + ";"

        //  ensemble de sessions qui ont lieu le jour interdit (Campus Day)
        var S0: Array[Int] = Array()

        for (i <- 0 until inst.DATA.nr_sessions) {
          if (inst.SOLUTION.SESSIONS.session_week(i) == semaine && inst.SOLUTION.SESSIONS.session_day(i) == jour) {
            S0 = S0 :+ i
          }
        }

        var GrapheDePrecedence: Array[Array[Int]] = Array.tabulate(inst.DATA.nr_sessions, inst.DATA.nr_sessions)((se1, se2) => 0)

        for (constraint <- inst.CONSTRAINTS) {
          if (constraint.constraint == "sequenced") {
            if (constraint.sessions.length > 1) {
              for (i1 <- 0 until constraint.sessions.length - 1) {
                GrapheDePrecedence(constraint.sessions(i1).set(constraint.sessions(i1).set.length - 1) - 1)(constraint.sessions(i1 + 1).set(0) - 1) = 1
              }
            }
            else {
              for (i <- 0 until constraint.sessions(0).set.length - 1) {
                GrapheDePrecedence(constraint.sessions(0).set(i) - 1)(constraint.sessions(0).set(i + 1) - 1) = 1
              }
            }
          }
        }

        val MaxIt: Int = 10
        var it: Int = 0

        var x_bdh: Array[CBLSIntVar] = null
        var x_bwd: Array[CBLSIntVar] = null
        var x_bw: Array[CBLSIntVar] = null
        var violation: Int = 1

        var continue = true

        while (it < MaxIt && violation > 0 && continue) {

          val tmp = CBLS(inst, S0, semaine, jour)

          x_bdh = tmp._1
          x_bwd = tmp._2
          x_bw = tmp._3
          violation = tmp._4

          continue = false
          for (se <- S0) {
            for (i <- 0 until GrapheDePrecedence(se).length) {
              if (GrapheDePrecedence(se)(i) == 1 && !S0.contains(i)) {
                S0 = S0 :+ i

                continue = true
              }
            }
          }

          it += 1
//          println("\n")
        }

        out += it + ";" + S0.length + ";"

//        print("trouvé solution:")
        if(violation == 0) {
//          println("oui")
          out += "oui"
          solution_trouve += 1
        } else {
//          println("non")
          out += "non"
          solution_non_trouve +=1
        }
        out += "\n"
        println()

      }
    }
    val file = new File("/home/etud/USP/USP/test2.csv")
    val bw = new BufferedWriter(new FileWriter(file))

    bw.write(out)

    bw.close()
    println(solution_trouve + " solution trouvé et " + solution_non_trouve + " solution non trouvé pour " + (solution_trouve+solution_non_trouve) + " journée")
  }
}
