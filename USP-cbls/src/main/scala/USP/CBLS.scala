package USP

import oscar.cbls
import oscar.cbls._
import oscar.cbls.lib.constraint._

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source

object CBLS extends CBLSModel{
  def CBLS(instance: InstanceUTP): Array[CBLSIntVar] = {

    for (constraint <- instance.CONSTRAINTS) {
      constraint.constraint match {
        //case "adjacent_rooms" => adjacent_rooms(constraint, instance)
        case "allowed_slots" | "allowedPeriod" => allowed_slots(constraint, instance)
        //case "allowed_rooms" => allowed_rooms(constraint, instance)
        case "assign_rooms" | "assignRoom" => assign_rooms(constraint, instance)
        case "assign_slot" => assign_slot(constraint, instance)
        case "assign_teachers" => assign_teachers(constraint, instance)
        case "different_daily_slot" => different_daily_slot(constraint, instance)
        case "different_day" => different_day(constraint, instance)
        case "different_rooms" => different_rooms(constraint, instance)
        case "different_slot" => different_slot(constraint, instance)
        case "different_teachers" => different_teachers(constraint, instance)
        case "different_week" | "differentWeek" => different_week(constraint, instance)
        case "different_weekday" | "differentWeekDay" => different_weekday(constraint, instance)
        case "different_weeklyslot" => different_weeklyslot(constraint, instance)
        case "forbidden_rooms" | "forbiddenRooms" => forbidden_rooms(constraint, instance)
        case "forbidden_slots" | "forbiddenPeriod" => forbidden_slots(constraint, instance)
        case "forbidden_teachers" => forbidden_teachers(constraint, instance)
        case "no_overlap" | "disjunct" => //no_overlap(constraint, instance)
        //case "pairwise_no_overlap" => pairwise_no_overlap(constraint, instance)
        case "periodic" => periodic(constraint, instance)
        //case "required_rooms" => required_rooms(constraint, instance)
        case "same_daily_slot" => same_daily_slot(constraint, instance)
        case "same_day" => same_day(constraint, instance)
        case "same_rooms" | "sameRooms" => same_rooms(constraint, instance)
        case "same_slot" | "sameSlot" | "sameSlots" => same_slot(constraint, instance)
        case "same_teachers" | "sameTeachers" => same_teachers(constraint, instance)
        case "same_week" | "sameWeek" => same_week(constraint, instance)
        case "same_weekday" | "sameWeekDay" => same_weekday(constraint, instance)
        case "same_weeklySlot" | "sameWeeklySlot" => same_weeklyslot(constraint, instance)
        case "sequenced" => sequenced(constraint, instance)
        case _ => println("constraint pas encore implémenter:" + constraint.constraint)
      }
    }

    val nr_slot = instance.DATA.nr_weeks * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day

    val range: Range = Range.inclusive(1, nr_slot)

    val x_slot = Array.tabulate(instance.DATA.nr_sessions)(
      session => CBLSIntVar((instance.SOLUTION.SESSIONS.session_week(session) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day + (instance.SOLUTION.SESSIONS.session_day(session) - 1) * instance.DATA.nr_slots_per_day + instance.SOLUTION.SESSIONS.session_dailyslot(session),
        range,
        "session" + session))

    val distanceCost: Array[Array[CBLSIntVar]] = Array.tabulate(instance.DATA.nr_sessions)(
      se1 => Array.tabulate(instance.DATA.nr_sessions)(
        se2 => if (se1 != se2) {
          CBLSIntVar(math.sqrt(math.pow(x_slot(se1).value - x_slot(se2).value, 2)).toInt)
        } else {
          CBLSIntVar(100000)
        }
      )
    )

    val distanceToNearestSlot = Array.tabulate(instance.DATA.nr_sessions)(
      session => CBLSIntVar(-2) //minNaive(distanceCost(session))
    )

    //c.post(sum(distanceToNearestSlot)===0)
    val obj = Objective(sum(distanceToNearestSlot))
    for (i <- 0 until instance.DATA.nr_sessions) {
      for (j <- 0 until instance.DATA.nr_sessions) {
        if (i != j)
          distanceCost(i)(j) <== abs(x_slot(i) - x_slot(j))
      }
      distanceToNearestSlot(i) <== minNaive(distanceCost(i))
    }
    //      distanceCost(0)(1) <== abs(x_slot(0)-x_slot(1))
    //      distanceCost(2)(0) <== abs(x_slot(0) - x_slot(2))
    //      distanceCost(1)(2) <== abs(x_slot(1) - x_slot(2))
    //
    //      distanceToNearestSlot(0) <== minNaive(distanceCost(0))
    //      distanceToNearestSlot(1) <== minNaive(distanceCost(1))
    //      distanceToNearestSlot(2) <== minNaive(distanceCost(2))
    //println(sum(distanceToNearestSlot))
    //c.post(EQ(sum(for(i <- 0 until instance.DATA.nr_sessions) yield minNaive(distanceCost(i))),0))

    var it: Int = 0
    val MaxIt: Int = 10

    s.close()

    //println(c.violation.value + " " + x_slot(0) + " " + x_slot(1) + " " + x_slot(2))

    //while(c.violation.value > 0 && it < MaxIt){
    while (it < MaxIt) {
      val se1 = it % instance.DATA.nr_sessions

      var bestOccurence = selectMin(range)(slot => assignVal(x_slot(se1), slot)(obj))
      //println(x_slot(0).value + " " + x_slot(1).value + " " + x_slot(2).value + " " + bestOccurence)
      x_slot(se1) := bestOccurence
//      for (i <- 0 until 3)
//        println(distanceToNearestSlot(i))
//              for(se2 <- 0 until instance.DATA.nr_sessions){
//                if(se1!=se2) {
//                  distanceCost(se1)(se2) := math.sqrt(math.pow(x_slot(se1).value - x_slot(se2).value, 2)).toInt
//                  distanceCost(se2)(se1) := math.sqrt(math.pow(x_slot(se1).value - x_slot(se2).value, 2)).toInt
//                }
//              }
//      println(c.violation)
//
//              for (i <- 0 until instance.DATA.nr_sessions) {
//                print(distanceCost(it%3)(i).value + " ")
//              }
//              println()
      it += 1
    }

    println()
    for (i <- 0 until instance.DATA.nr_sessions) {
      println(x_slot(i).value)
    }

    println()

    println("Le nombre de violation est de: " + c.violation.value)
    return x_slot

  }

  def adjacent_rooms(constraint: constraint, instance: InstanceUTP): Unit = {

  }

  def allowed_slots(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i <- constraint.sessions(0).set) {
      //println(instance.RULES.parameter_value(constraint.parameters(0)-1)(0).toString.toInt+"<"+(instance.SOLUTION.SESSIONS.session_dailyslot(i-1) + ((instance.SOLUTION.SESSIONS.session_day(i-1) - 1) * instance.DATA.nr_slots_per_day) + ((instance.SOLUTION.SESSIONS.session_week(i-1) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day))+"<"+instance.RULES.parameter_value(constraint.parameters(1)-1)(0).toString.toInt)
      if (constraint.parameters.length > 2) {
        instance.RULES.parameter_value(constraint.parameters(2) - 1)(0).toString match {
          case "\"day\"" => c.post(GE(instance.SOLUTION.SESSIONS.session_dailyslot(i - 1), instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt))
            c.post(LE(instance.SOLUTION.SESSIONS.session_dailyslot(i - 1), instance.RULES.parameter_value(constraint.parameters(1) - 1)(0).toString.toInt))
          case "\"week\"" => c.post(GE(instance.SOLUTION.SESSIONS.session_dailyslot(i - 1) + ((instance.SOLUTION.SESSIONS.session_day(i - 1) - 1) * instance.DATA.nr_slots_per_day), instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt))
            c.post(LE(instance.SOLUTION.SESSIONS.session_dailyslot(i - 1) + ((instance.SOLUTION.SESSIONS.session_day(i - 1) - 1) * instance.DATA.nr_slots_per_day), instance.RULES.parameter_value(constraint.parameters(1) - 1)(0).toString.toInt))
          case "\"global\"" => c.post(GE(instance.SOLUTION.SESSIONS.session_dailyslot(i - 1) + ((instance.SOLUTION.SESSIONS.session_day(i - 1) - 1) * instance.DATA.nr_slots_per_day) + ((instance.SOLUTION.SESSIONS.session_week(i - 1) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day), instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt))
            c.post(LE(instance.SOLUTION.SESSIONS.session_dailyslot(i - 1) + ((instance.SOLUTION.SESSIONS.session_day(i - 1) - 1) * instance.DATA.nr_slots_per_day) + ((instance.SOLUTION.SESSIONS.session_week(i - 1) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day), instance.RULES.parameter_value(constraint.parameters(1) - 1)(0).toString.toInt))
          case _ =>
        }
      }
      else {
        c.post(GE(instance.SOLUTION.SESSIONS.session_dailyslot(i - 1) + ((instance.SOLUTION.SESSIONS.session_day(i - 1) - 1) * instance.DATA.nr_slots_per_day) + ((instance.SOLUTION.SESSIONS.session_week(i - 1) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day), instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt))
        c.post(LE(instance.SOLUTION.SESSIONS.session_dailyslot(i - 1) + ((instance.SOLUTION.SESSIONS.session_day(i - 1) - 1) * instance.DATA.nr_slots_per_day) + ((instance.SOLUTION.SESSIONS.session_week(i - 1) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day), instance.RULES.parameter_value(constraint.parameters(1) - 1)(0).toString.toInt))
      }
    }
  }

  def allowed_rooms(constraint: constraint, instance: InstanceUTP): Unit = {

  }

  def assign_rooms(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i <- constraint.sessions(0).set) {
      c.post(EQ(instance.SOLUTION.SESSIONS.session_rooms(i - 1).set(0), instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt))
    }
  }

  def assign_slot(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i <- constraint.sessions(0).set) {
      c.post(EQ(instance.SOLUTION.SESSIONS.session_dailyslot(i - 1) + ((instance.SOLUTION.SESSIONS.session_day(i - 1) - 1) * instance.DATA.nr_slots_per_day) + ((instance.SOLUTION.SESSIONS.session_week(i - 1) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day), instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt))
    }
  }

  def assign_teachers(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i <- constraint.sessions(0).set) {
      c.post(EQ(instance.SOLUTION.SESSIONS.session_teachers(i - 1).set(0), instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt))
    }
  }

  def different_daily_slot(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- constraint.sessions(0).set) {
      for (i2 <- constraint.sessions(0).set) {
        if (i1 != i2) {
          c.post(NE(CBLSIntVar(instance.SOLUTION.SESSIONS.session_dailyslot(i1 - 1)), CBLSIntVar(instance.SOLUTION.SESSIONS.session_dailyslot(i2 - 1))))
        }
      }
    }
  }

  def different_day(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- constraint.sessions(0).set) {
      for (i2 <- constraint.sessions(0).set) {
        if (i1 != i2) {
          c.post(NE(CBLSIntVar(instance.SOLUTION.SESSIONS.session_day(i1 - 1) + instance.DATA.nr_days_per_week * (instance.SOLUTION.SESSIONS.session_week(i1 - 1) - 1)), CBLSIntVar(instance.SOLUTION.SESSIONS.session_day(i2 - 1) + instance.DATA.nr_days_per_week * (instance.SOLUTION.SESSIONS.session_week(i2 - 1) - 1))))
        }
      }
    }
  }

  def different_rooms(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- constraint.sessions(0).set) {
      for (i2 <- constraint.sessions(0).set) {
        if (i1 != i2) {
          c.post(NE(instance.SOLUTION.SESSIONS.session_rooms(i1 - 1).set(0), instance.SOLUTION.SESSIONS.session_rooms(i2 - 1).set(0)))
        }
      }
    }
  }

  def different_slot(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- constraint.sessions(0).set) {
      for (i2 <- constraint.sessions(0).set) {
        if (i1 != i2) { // améliorer
          c.post(NE(instance.SOLUTION.SESSIONS.session_dailyslot(i1 - 1) + ((instance.SOLUTION.SESSIONS.session_day(i1 - 1) - 1) * instance.DATA.nr_slots_per_day) + ((instance.SOLUTION.SESSIONS.session_week(i1 - 1) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day), instance.SOLUTION.SESSIONS.session_dailyslot(i2 - 1) + ((instance.SOLUTION.SESSIONS.session_day(i2 - 1) - 1) * instance.DATA.nr_slots_per_day) + ((instance.SOLUTION.SESSIONS.session_week(i2 - 1) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day)))
        }
      }
    }
  }

  def different_teachers(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- constraint.sessions(0).set) {
      for (i2 <- constraint.sessions(0).set) {
        if (i1 != i2) { // améliorer
          c.post(NE(CBLSIntVar(instance.SOLUTION.SESSIONS.session_teachers(i1 - 1).set(0)), CBLSIntVar(instance.SOLUTION.SESSIONS.session_teachers(i2 - 1).set(0))))
        }
      }
    }
  }

  def different_week(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- constraint.sessions(0).set) {
      for (i2 <- constraint.sessions(0).set) {
        if (i1 != i2) {
          c.post(NE(CBLSIntVar(instance.SOLUTION.SESSIONS.session_week(i1 - 1)), CBLSIntVar(instance.SOLUTION.SESSIONS.session_week(i2 - 1))))
        }
      }
    }
  }

  def different_weekday(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- constraint.sessions(0).set) {
      for (i2 <- constraint.sessions(0).set) {
        if (i1 != i2) {
          //c.post(EQ(instance.SOLUTION.SESSIONS.session_week(i1 - 1), instance.SOLUTION.SESSIONS.session_week(i2 - 1)))
          c.post(NE(CBLSIntVar(instance.SOLUTION.SESSIONS.session_day(i1 - 1)), CBLSIntVar(instance.SOLUTION.SESSIONS.session_day(i2 - 1))))
        }
      }
    }
  }

  def different_weeklyslot(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- constraint.sessions(0).set) {
      for (i2 <- constraint.sessions(0).set) {
        if (i1 != i2) {
          c.post(NE(CBLSIntVar(instance.SOLUTION.SESSIONS.session_dailyslot(i1 - 1) + instance.DATA.nr_slots_per_day * (instance.SOLUTION.SESSIONS.session_day(i1 - 1) - 1)), CBLSIntVar(instance.SOLUTION.SESSIONS.session_week(i2 - 1) + instance.DATA.nr_slots_per_day * (instance.SOLUTION.SESSIONS.session_day(i1 - 1) - 1))))
        }
      }
    }
  }

  def forbidden_rooms(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i <- constraint.sessions(0).set) {
      for (j <- instance.RULES.parameter_value(constraint.parameters(0) - 1)) {
        c.post(NE(CBLSIntVar(instance.SOLUTION.SESSIONS.session_rooms(i - 1).set(0)), CBLSIntVar(j.toString.toInt)))
      }
    }
  }

  def forbidden_slots(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i <- constraint.sessions(0).set) {
      if (constraint.parameters.length > 2) {
        instance.RULES.parameter_value(constraint.parameters(2) - 1)(0).toString match {
          case "\"day\"" => c.post(NE(if (instance.SOLUTION.SESSIONS.session_dailyslot(i - 1) <= instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt)
            CBLSIntVar(1) else CBLSIntVar(0),
            if (instance.SOLUTION.SESSIONS.session_dailyslot(i - 1) >= instance.RULES.parameter_value(constraint.parameters(1) - 1)(0).toString.toInt)
              CBLSIntVar(1) else CBLSIntVar(0)))
          case "\"week\"" => c.post(NE(if (instance.SOLUTION.SESSIONS.session_dailyslot(i - 1) + ((instance.SOLUTION.SESSIONS.session_day(i - 1) - 1) * instance.DATA.nr_slots_per_day) <= instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt)
            CBLSIntVar(1) else CBLSIntVar(0),
            if (instance.SOLUTION.SESSIONS.session_dailyslot(i - 1) + ((instance.SOLUTION.SESSIONS.session_day(i - 1) - 1) * instance.DATA.nr_slots_per_day) >= instance.RULES.parameter_value(constraint.parameters(1) - 1)(0).toString.toInt)
              CBLSIntVar(1) else CBLSIntVar(0)))
          case "\"global\"" => c.post(NE(if (instance.SOLUTION.SESSIONS.session_dailyslot(i - 1) + ((instance.SOLUTION.SESSIONS.session_day(i - 1) - 1) * instance.DATA.nr_slots_per_day) + ((instance.SOLUTION.SESSIONS.session_week(i - 1) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day) <= instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt)
            CBLSIntVar(1) else CBLSIntVar(0),
            if (instance.SOLUTION.SESSIONS.session_dailyslot(i - 1) + ((instance.SOLUTION.SESSIONS.session_day(i - 1) - 1) * instance.DATA.nr_slots_per_day) + ((instance.SOLUTION.SESSIONS.session_week(i - 1) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day) >= instance.RULES.parameter_value(constraint.parameters(1) - 1)(0).toString.toInt)
              CBLSIntVar(1) else CBLSIntVar(0)))
          case _ =>
        }
      }
      else {
        c.post(NE(if (instance.SOLUTION.SESSIONS.session_dailyslot(i - 1) + ((instance.SOLUTION.SESSIONS.session_day(i - 1) - 1) * instance.DATA.nr_slots_per_day) + ((instance.SOLUTION.SESSIONS.session_week(i - 1) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day) <= instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt)
          CBLSIntVar(1) else CBLSIntVar(0),
          if (instance.SOLUTION.SESSIONS.session_dailyslot(i - 1) + ((instance.SOLUTION.SESSIONS.session_day(i - 1) - 1) * instance.DATA.nr_slots_per_day) + ((instance.SOLUTION.SESSIONS.session_week(i - 1) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day) >= instance.RULES.parameter_value(constraint.parameters(1) - 1)(0).toString.toInt)
            CBLSIntVar(1) else CBLSIntVar(0)))
      }
    }
  }

  def forbidden_teachers(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i <- constraint.sessions(0).set) {
      for (j <- instance.RULES.parameter_value(constraint.parameters(0) - 1)) {
        c.post(NE(CBLSIntVar(instance.SOLUTION.SESSIONS.session_teachers(i - 1).set(0)), CBLSIntVar(j.toString.toInt)))
      }
    }
  }

  def no_overlap(constraint: constraint, instance: InstanceUTP): Unit = {

  }

  def pairwise_no_overlap(constraint: constraint, instance: InstanceUTP): Unit = {

  }

  def periodic(constraint: constraint, instance: InstanceUTP): Unit = {
    var value: Int = instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt
    var unit: String = instance.RULES.parameter_value(constraint.parameters(1) - 1)(0).toString

    unit match {
      case "\"day\"" => // à revoir
        for (i <- 0 until constraint.sessions(0).set.length - 1) {
          c.post(EQ(instance.SOLUTION.SESSIONS.session_week(constraint.sessions(0).set(i) - 1), instance.SOLUTION.SESSIONS.session_week(constraint.sessions(0).set(i + 1) - 1)))
          c.post(EQ(instance.SOLUTION.SESSIONS.session_day(constraint.sessions(0).set(i) - 1) + value, instance.SOLUTION.SESSIONS.session_day(constraint.sessions(0).set(i + 1) - 1)))
        }
      case "\"week\"" =>
        for (i <- 0 until constraint.sessions(0).set.length - 1) {
          c.post(EQ(instance.SOLUTION.SESSIONS.session_dailyslot(constraint.sessions(0).set(i) - 1), instance.SOLUTION.SESSIONS.session_dailyslot(constraint.sessions(0).set(i + 1) - 1)))
          c.post(EQ(instance.SOLUTION.SESSIONS.session_day(constraint.sessions(0).set(i) - 1), instance.SOLUTION.SESSIONS.session_day(constraint.sessions(0).set(i + 1) - 1)))
          c.post(EQ(instance.SOLUTION.SESSIONS.session_week(constraint.sessions(0).set(i) - 1) + value, instance.SOLUTION.SESSIONS.session_week(constraint.sessions(0).set(i + 1) - 1)))
        }
      case _ =>

    }
  }

  def required_rooms(constraint: constraint, instance: InstanceUTP): Unit = {

  }

  def same_daily_slot(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- constraint.sessions(0).set) {
      for (i2 <- constraint.sessions(0).set) {
        if (i1 != i2) {
          c.post(EQ(instance.SOLUTION.SESSIONS.session_dailyslot(i1 - 1), instance.SOLUTION.SESSIONS.session_dailyslot(i2 - 1)))
        }
      }
    }
  }

  def same_day(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- constraint.sessions(0).set) {
      for (i2 <- constraint.sessions(0).set) {
        if (i1 != i2) {
          c.post(EQ(instance.SOLUTION.SESSIONS.session_day(i1 - 1) + instance.DATA.nr_days_per_week * (instance.SOLUTION.SESSIONS.session_week(i1 - 1) - 1), instance.SOLUTION.SESSIONS.session_day(i2 - 1) + instance.DATA.nr_days_per_week * (instance.SOLUTION.SESSIONS.session_week(i2 - 1) - 1)))
        }
      }
    }
  }

  def same_rooms(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- constraint.sessions(0).set) {
      for (i2 <- constraint.sessions(0).set) {
        if (i1 != i2) {
          c.post(EQ(CBLSIntVar(instance.SOLUTION.SESSIONS.session_rooms(i1 - 1).set(0)), CBLSIntVar(instance.SOLUTION.SESSIONS.session_rooms(i2 - 1).set(0))))
        }
      }
    }
  }

  def same_slot(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- constraint.sessions(0).set) {
      for (i2 <- constraint.sessions(0).set) {
        if (i1 != i2) { // améliorer
          c.post(EQ(instance.SOLUTION.SESSIONS.session_dailyslot(i1 - 1) + ((instance.SOLUTION.SESSIONS.session_day(i1 - 1) - 1) * instance.DATA.nr_slots_per_day) + ((instance.SOLUTION.SESSIONS.session_week(i1 - 1) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day), instance.SOLUTION.SESSIONS.session_dailyslot(i2 - 1) + ((instance.SOLUTION.SESSIONS.session_day(i2 - 1) - 1) * instance.DATA.nr_slots_per_day) + ((instance.SOLUTION.SESSIONS.session_week(i2 - 1) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day)))
          //println(instance.SOLUTION.SESSIONS.session_week(i1-1)+" "+instance.SOLUTION.SESSIONS.session_day(i1-1)+" "+instance.SOLUTION.SESSIONS.session_dailyslot(i1-1)+"->"+instance.SOLUTION.SESSIONS.session_week(i2-1)+" "+instance.SOLUTION.SESSIONS.session_day(i2-1)+" "+instance.SOLUTION.SESSIONS.session_dailyslot(i2-1))
        }
      }
    }
  }

  def same_teachers(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- constraint.sessions(0).set) {
      for (i2 <- constraint.sessions(0).set) {
        if (i1 != i2) { // améliorer
          c.post(EQ(CBLSIntVar(instance.SOLUTION.SESSIONS.session_teachers(i1 - 1).set(0)), CBLSIntVar(instance.SOLUTION.SESSIONS.session_teachers(i2 - 1).set(0))))
        }
      }
    }
  }

  def same_week(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- constraint.sessions(0).set) {
      for (i2 <- constraint.sessions(0).set) {
        if (i1 != i2) {
          c.post(EQ(CBLSIntVar(instance.SOLUTION.SESSIONS.session_week(i1 - 1)), CBLSIntVar(instance.SOLUTION.SESSIONS.session_week(i2 - 1))))
        }
      }
    }
  }

  def same_weekday(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- 0 until constraint.sessions(0).set.length - 1) {
      for (i2 <- i1 + 1 until constraint.sessions(0).set.length) {
        //println(constraint.rule+": i1 -> "+instance.SOLUTION.SESSIONS.session_day(constraint.sessions(0).set(i1) - 1)+"; i2 -> "+instance.SOLUTION.SESSIONS.session_day(constraint.sessions(0).set(i2) - 1))
        c.post(EQ(CBLSIntVar(instance.SOLUTION.SESSIONS.session_day(constraint.sessions(0).set(i1) - 1)), CBLSIntVar(instance.SOLUTION.SESSIONS.session_day(constraint.sessions(0).set(i2) - 1))))
      }
    }
  }

  def same_weeklyslot(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- constraint.sessions(0).set) {
      for (i2 <- constraint.sessions(0).set) {
        if (i1 != i2) {
          if (constraint.sessions(0).set.length > 1) {
            c.post(EQ(instance.SOLUTION.SESSIONS.session_dailyslot(i1 - 1), instance.SOLUTION.SESSIONS.session_dailyslot(i2 - 1)))
            c.post(EQ(instance.SOLUTION.SESSIONS.session_day(i1 - 1), instance.SOLUTION.SESSIONS.session_day(i2 - 1)))
          }
        }
      }
    }
  }

  def sequenced(constraint: constraint, instance: InstanceUTP): Unit = {
    if (constraint.sessions.length > 1) {
      for (i1 <- constraint.sessions(0).set) {
        for (i2 <- constraint.sessions(1).set) {
          c.post(L(instance.SOLUTION.SESSIONS.session_dailyslot(i1 - 1) + ((instance.SOLUTION.SESSIONS.session_day(i1 - 1) - 1) * instance.DATA.nr_slots_per_day) + ((instance.SOLUTION.SESSIONS.session_week(i1 - 1) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day), instance.SOLUTION.SESSIONS.session_dailyslot(i2 - 1) + ((instance.SOLUTION.SESSIONS.session_day(i2 - 1) - 1) * instance.DATA.nr_slots_per_day) + ((instance.SOLUTION.SESSIONS.session_week(i2 - 1) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day)))
        }
      }
    }
  }

  def write_solution_file(instance: InstanceUTP, x_slot: Array[CBLSIntVar], fileName: String): Unit = {
    var out: String = ""

    val lines = Source.fromFile(fileName).getLines().toList

    var i: Int = 0

    while (lines(i).trim() != "<sessions>" && i < lines.length - 1) {
      out += lines(i) + "\n"
      i += 1
    }

    out += print_xml(instance, x_slot)
    out += "\n</solution>\n</timetabling>"

    val file = new File("/home/etud/USP/USP/instance/test_v.xml")
    val bw = new BufferedWriter(new FileWriter(file))

    bw.write(out)

    bw.close()
  }

  def print_xml(instance: InstanceUTP, x_slot: Array[CBLSIntVar]): String = {
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
      val slotToTime = slot2time(instance, x_slot(i).value)
      out += "<session rank= \"" + instance.SOLUTION.SESSIONS.session_rank(i) + "\" class=\"" + instance.DATA.class_name(instance.SOLUTION.SESSIONS.session_class(i) - 1) + "\">\n"
      out += "    <startingSlot dailySlot=\"" + slotToTime(3) + "\" day=\"" + slotToTime(2) + "\" week=\"" + slotToTime(1) + "\" />\n"
      out += "    <rooms>\n" + print_xml_room_solution(instance, i) + "    </rooms>\n"
      out += "    <teachers>\n" + print_xml_teacher_solution(instance, i) + "    </teachers>\n"
      out += "</session>\n"
    }

    out += "</sessions>\n"

    return out
  }

  def slot2time(instance: InstanceUTP, slot: Int): Array[Int] = {
    var tab = new Array[Int](4)

    tab(0) = slot

    val week: Int = slot / (instance.DATA.nr_slots_per_day * instance.DATA.nr_days_per_week)
    val slot2: Int = slot - (week * (instance.DATA.nr_slots_per_day * instance.DATA.nr_days_per_week))
    val day: Int = slot2 / instance.DATA.nr_slots_per_day
    val dailySlot: Int = slot2 - (day * instance.DATA.nr_slots_per_day)

    tab(1) = week + 1
    tab(2) = day + 1
    tab(3) = dailySlot

    return tab
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
