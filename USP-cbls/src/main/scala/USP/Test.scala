package USP

import java.io._
import scala.io.Source
import USP.CBLS._
import oscar.cbls._

import scala.collection.mutable.ListBuffer

object Test {

  def main(args: Array[String]): Unit = {

    var original: Boolean = false
    var semaine: Int = 1
    var jour: Int = 1

    if(args.length > 2){
      for(i <- 2 until args.length){
        args(i) match {
          case "-o" => original = true
          case "-JI" => semaine = args(i + 1).toInt
            jour = args(i + 2).toInt
          case _ =>
        }
      }
    }

    val parser = new ConverterJson(args(0))
    val inst = parser.readJson()

    if(original){
      write_solution_file(inst, fileName = args(1), original = original)
    }
    else {

//      ensemble de sessions qui ont lieu le jour interdit (Campus Day)
      var S0: Array[Int] = Array()

      for (i <- 0 until inst.DATA.nr_sessions) {
        if (inst.SOLUTION.SESSIONS.session_week(i) == semaine && inst.SOLUTION.SESSIONS.session_day(i) == jour) {
          S0 = S0 :+ i
        }
      }

      var GrapheDePrecedence: Array[Array[Int]] = Array.tabulate(inst.DATA.nr_sessions, inst.DATA.nr_sessions)((se1, se2) => 0)

      for (constraint <- inst.CONSTRAINTS) {
        if(constraint.constraint == "sequenced"){
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
        println("boucle " + it + ":\n")

        println("Séance libérer:")
        for (i <- S0) {
          print(i + " ")
        }
        println("\n")

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
        println("\n")
      }

      write_solution_file(inst, x_bdh, x_bwd, x_bw, args(1), original)
    }
  }
}
