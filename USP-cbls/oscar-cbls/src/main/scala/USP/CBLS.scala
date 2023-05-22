import oscar.cbls
import oscar.cbls._
import oscar.cbls.lib.constraint._

package USP {

  object CBLS extends CBLSModel {

    def CBLS(instance: InstanceUTP): Unit = {

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

      s.close()
      println("Le nombre de violation est de: " + c.violation.value)
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
      print(instance.DATA.room_name(instance.SOLUTION.SESSIONS.session_rooms(constraint.sessions(0).set(0) - 1).set(0) - 1) + "->")
      for (i <- constraint.sessions(1).set) {
        print(instance.DATA.room_name(instance.SOLUTION.SESSIONS.session_rooms(i - 1).set(0)) + "; ")
      }
      println()
    }

    def pairwise_no_overlap(constraint: constraint, instance: InstanceUTP): Unit = {

    }

    def periodic(constraint: constraint, instance: InstanceUTP): Unit = {
      var value: Int = instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt
      var unit: String = instance.RULES.parameter_value(constraint.parameters(1) - 1)(0).toString

      unit match {
        case "\"day\"" =>
          for (i <- 0 until constraint.sessions(0).set.length - 1) {
            c.post(EQ(instance.SOLUTION.SESSIONS.session_week(constraint.sessions(0).set(i) - 1), instance.SOLUTION.SESSIONS.session_week(constraint.sessions(0).set(i + 1) - 1)))
            c.post(EQ(instance.SOLUTION.SESSIONS.session_day(constraint.sessions(0).set(i) - 1) + value, instance.SOLUTION.SESSIONS.session_day(constraint.sessions(0).set(i + 1) - 1)))
          }
        case "\"week\"" =>
          for (i <- 0 until constraint.sessions(0).set.length - 1) {
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

  }
}