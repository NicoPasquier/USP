package USP

import oscar.cbls._
import oscar.cbls.lib.search.LinearSelectors
import oscar.cbls.lib.constraint._
import oscar.cbls.modeling.{CombinatorsAPI, Constraints, LogicInvariants, MinMaxInvariants, NumericInvariants, SeqInvariants, SetInvariants, StandardNeighborhoods}
import oscar.cbls.util.StopWatch

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source

object CBLS extends LinearSelectors
  with Constraints
  with LogicInvariants
  with MinMaxInvariants
  with NumericInvariants
  with SetInvariants
  with SeqInvariants
  with StopWatch
  with CombinatorsAPI
  with StandardNeighborhoods
  {

  def CBLS(instance: InstanceUTP, S0: Array[Int], semaine: Int, jour:Int): (Array[CBLSIntVar], Array[CBLSIntVar], Array[CBLSIntVar], Int) = {

    val s: Store = Store(false, None, true)
    val c: ConstraintSystem = ConstraintSystem(s)

    startWatch()

    val class_part: Array[Int] = this.class_part(instance)

    val nr_slot = instance.DATA.nr_weeks * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day

    // Echelles de temps
    val H_w: Range = Range.inclusive(1, instance.DATA.nr_weeks)
    val H_wd: Range = Range.inclusive(1, instance.DATA.nr_days_per_week)
    val H_dh = Range.inclusive(1, instance.DATA.nr_slots_per_day)

    val H_d: Range = Range.inclusive(1, instance.DATA.nr_weeks * instance.DATA.nr_days_per_week)
    val H_wh: Range = Range.inclusive(1, instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day)
    val H_h: Range = Range.inclusive(1, instance.DATA.nr_weeks * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day)

    // Variables primitives temporelles
    val x_bw = Array.tabulate(instance.DATA.nr_sessions)(
      session => CBLSIntVar(s, instance.SOLUTION.SESSIONS.session_week(session),
        H_w,
        "créneau semaine de la session " + session))

    val x_bwd = Array.tabulate(instance.DATA.nr_sessions)(
      session => CBLSIntVar(s, instance.SOLUTION.SESSIONS.session_day(session),
        H_wd,
        "créneau jour de la session " + session))

    val x_bdh = Array.tabulate(instance.DATA.nr_sessions)(
      session => CBLSIntVar(s, instance.SOLUTION.SESSIONS.session_dailyslot(session),
        H_dh,
        "créneau horaire de la session " + session))


    // Variables primitives d'allocation de ressources


    // Invariants temporels
    val y_bw = Array.tabulate(instance.DATA.nr_sessions)(
      session => CBLSIntVar(s, instance.SOLUTION.SESSIONS.session_week(session),
        H_w,
        "semaine de démarage de la session " + session))

    val y_bwd = Array.tabulate(instance.DATA.nr_sessions)(
      session => CBLSIntVar(s, instance.SOLUTION.SESSIONS.session_day(session),
        H_wd,
        "jour de démarrage de la session " + session))

    val y_bdh = Array.tabulate(instance.DATA.nr_sessions)(
      session => CBLSIntVar(s, instance.SOLUTION.SESSIONS.session_dailyslot(session),
        H_dh,
        "horaire de démarrage de la session " + session))

    val y_ew = Array.tabulate(instance.DATA.nr_sessions)(
      session => CBLSIntVar(s, instance.SOLUTION.SESSIONS.session_week(session),
        H_w,
        "semaine de fin de la session " + session))

    val y_ewd = Array.tabulate(instance.DATA.nr_sessions)(
      session => CBLSIntVar(s, instance.SOLUTION.SESSIONS.session_day(session),
        H_wd,
        "jour de fin de la session " + session))

    val y_edh = Array.tabulate(instance.DATA.nr_sessions)(
      session => CBLSIntVar(s, instance.SOLUTION.SESSIONS.session_dailyslot(session) + instance.DATA.part_session_length(class_part(instance.SOLUTION.SESSIONS.session_class(session) - 1) - 1),
        H_dh,
        "horaire de fin de la session " + session))

    val y_bh = Array.tabulate(instance.DATA.nr_sessions)(
      session => CBLSIntVar(s, (instance.SOLUTION.SESSIONS.session_week(session) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day + (instance.SOLUTION.SESSIONS.session_day(session) - 1) * instance.DATA.nr_slots_per_day + instance.SOLUTION.SESSIONS.session_dailyslot(session),
        H_h,
        "date de démarrage de la session " + session))

    val y_bwh = Array.tabulate(instance.DATA.nr_sessions)(
      session => CBLSIntVar(s, (instance.SOLUTION.SESSIONS.session_day(session)-1) * instance.DATA.nr_slots_per_day + instance.SOLUTION.SESSIONS.session_dailyslot(session),
        H_wh,
        "horaire hebdomadaire de démarrage de la session " + session))

    val y_bd = Array.tabulate(instance.DATA.nr_sessions)(
      session => CBLSIntVar(s, (instance.SOLUTION.SESSIONS.session_day(session) - 1) * instance.DATA.nr_slots_per_day + instance.SOLUTION.SESSIONS.session_dailyslot(session),
        H_d,
        "journée de démarage de la session " + session))

    val y_eh = Array.tabulate(instance.DATA.nr_sessions)(
      session => CBLSIntVar(s, (instance.SOLUTION.SESSIONS.session_week(session) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day + (instance.SOLUTION.SESSIONS.session_day(session) - 1) * instance.DATA.nr_slots_per_day + instance.SOLUTION.SESSIONS.session_dailyslot(session) + instance.DATA.part_session_length(class_part(instance.SOLUTION.SESSIONS.session_class(session) - 1) - 1),
        H_h,
        "date de fin de la session " + session))

    val y_ewh = Array.tabulate(instance.DATA.nr_sessions)(
      session => CBLSIntVar(s, (instance.SOLUTION.SESSIONS.session_day(session) - 1) * instance.DATA.nr_slots_per_day + instance.SOLUTION.SESSIONS.session_dailyslot(session),
        H_wh,
        "horaire hebdomadaire de fin de la session " + session))

    val y_ed = Array.tabulate(instance.DATA.nr_sessions)(
      session => CBLSIntVar(s, (instance.SOLUTION.SESSIONS.session_day(session) - 1) * instance.DATA.nr_slots_per_day + instance.SOLUTION.SESSIONS.session_dailyslot(session),
        H_d,
        "journée de fin de la session " + session))



    for (i <- 0 until instance.DATA.nr_sessions) {
      y_bw(i) <== x_bw(i)
      y_bwd(i) <== x_bwd(i)
      y_bdh(i) <== x_bdh(i)

      y_ew(i) <== y_bw(i)
      y_ewd(i) <== y_bwd(i)
      y_edh(i) <== y_bdh(i) + instance.DATA.part_session_length(class_part(instance.SOLUTION.SESSIONS.session_class(i) - 1) - 1)

      y_bh(i) <== ((y_bw(i) - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day) + ((y_bwd(i) - 1) * instance.DATA.nr_slots_per_day) + y_bdh(i)
      y_bwh(i) <== ((y_bh(i) - 1) % (instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day)) + 1
      y_bd(i) <== ((y_bh(i) - 1) / instance.DATA.nr_slots_per_day) + 1
      y_eh(i) <== y_bh(i) + instance.DATA.part_session_length(class_part(instance.SOLUTION.SESSIONS.session_class(i) - 1) - 1)
      y_ewh(i) <== y_bwh(i)
      y_ed(i) <== y_bd(i)
    }

    // le slot d'une session doit correspondre à l'une des ces dailyslots
    for(i <- 0 until instance.DATA.nr_sessions){
      c.post(BelongsToConst(y_bdh(i), instance.DATA.part_dailyslots(class_part(instance.SOLUTION.SESSIONS.session_class(i) - 1) - 1).set.toSet))
    }

    var periodic_index: Array[Int] = Array()

    var index = 0
    for (constraint <- instance.CONSTRAINTS) {
      constraint.constraint match {
        //case "adjacent_rooms" => adjacent_rooms(constraint, instance)
        case "allowed_slots" | "allowedPeriod" => allowed_slots(c, constraint, instance, x_bdh, y_bwd, y_bw, y_bh, y_bwh)
        case "allowed_rooms" => //allowed_rooms(constraint, instance)
        case "assign_rooms" | "assignRoom" => //assign_rooms(constraint, instance)
        case "assign_slot" => assign_slot(c, constraint, instance,y_bh, y_bdh, y_bwd, y_bw)
        case "assign_teachers" => //assign_teachers(constraint, instance)
        case "different_daily_slot" => different_daily_slot(c, constraint, instance, y_bdh)
        case "different_day" => different_day(c, constraint, instance, y_bd)
        case "different_rooms" => //different_rooms(constraint, instance)
        case "different_slot" => different_slot(c, constraint, instance, y_bh)
        case "different_teachers" => //different_teachers(constraint, instance)
        case "different_week" | "differentWeek" => different_week(c, constraint, instance, y_bw)
        case "different_weekday" | "differentWeekDay" => different_weekday(c, constraint, instance, y_bwd)
        case "different_weeklyslot" => different_weeklyslot(c, constraint, instance, y_bwh)
        case "forbidden_rooms" | "forbiddenRooms" => //forbidden_rooms(constraint, instance)
        case "forbidden_slots" | "forbiddenPeriod" => forbidden_slots(c, s, constraint, instance, class_part, y_bdh, y_bwd, y_bw, y_bh, y_bwh)
        case "forbidden_teachers" => //forbidden_teachers(constraint, instance)
        case "no_overlap" | "noOverlap" => no_overlap(c, constraint, instance, class_part, y_bh)
        //case "pairwise_no_overlap" => pairwise_no_overlap(constraint, instance)
        case "periodic" => sequenced(c, constraint, instance, S0, y_bh, y_eh)
          periodic_index = periodic_index :+ (index)
        case "required_rooms" => //required_rooms(constraint, instance)
        case "same_daily_slot" => same_daily_slot(c, constraint, instance, S0, y_bdh)
        case "same_day" => same_day(c, constraint, instance, S0, y_bd)
        case "same_rooms" | "sameRooms" => //same_rooms(constraint, instance)
        case "same_slot" | "sameSlot" => same_slot(c, constraint, instance, S0, y_bh)
        case "same_teachers" | "sameTeachers" => //same_teachers(constraint, instance)
        case "same_week" | "sameWeek" => same_week(c, constraint, instance, S0, y_bw)
        case "same_weekday" | "sameWeekDay" => same_weekday(c, constraint, instance, S0, y_bwd)
        case "same_weeklySlot" | "sameWeeklySlot" => same_weeklyslot(c, constraint, instance, S0, y_bwh)
        case "sequenced" => sequenced(c, constraint, instance, S0, y_bh, y_eh)
        case _ => //println("constraint pas encore implémenter:" + constraint.constraint)
      }
      index += 1
    }

    // distance du voisin le plus proche
//    val distanceCost: Array[Array[CBLSIntVar]] = Array.tabulate(instance.DATA.nr_sessions)(
//      se1 => Array.tabulate(instance.DATA.nr_sessions)(
//        se2 => if (se1 != se2) {
//          CBLSIntVar(math.sqrt(math.pow(y_bh(se1).value - y_bh(se2).value, 2)).toInt)
//        } else {
//          CBLSIntVar(100000)
//        }
//      )
//    )

//    val distanceToNearestSlot = Array.tabulate(instance.DATA.nr_sessions)(
//      session => CBLSIntVar(-2) //minNaive(distanceCost(session))
//    )
    //c.post(sum(distanceToNearestSlot)===0)

    var maxSession: Int = 0


    for(i <- S0){
      for(j <- periodic_index){
        if (instance.CONSTRAINTS(j).sessions(0).set.contains(i + 1)) {
          if (maxSession < instance.CONSTRAINTS(j).sessions(0).set.length) {
            maxSession = instance.CONSTRAINTS(j).sessions(0).set.length
          }
        }
      }
      c.post(y_bd(i) !== (semaine - 1) * instance.DATA.nr_days_per_week + jour)
    }

    val distanceJourInterdit: Array[CBLSIntVar] = Array.tabulate(instance.DATA.nr_sessions)(
      p => CBLSIntVar(s, 0)
    )

    c.close()

//    for (i <- 0 until instance.DATA.nr_sessions) {
//      for (j <- 0 until instance.DATA.nr_sessions) {
//        if (i != j)
//          distanceCost(i)(j) <== abs(x_slot(i) - x_slot(j))
//      }
//      distanceToNearestSlot(i) <== minNaive(distanceCost(i))
//    }

    for(i <- S0){
      val y_bdInit = (instance.SOLUTION.SESSIONS.session_week(i) - 1) * instance.DATA.nr_days_per_week + instance.SOLUTION.SESSIONS.session_day(i)
      distanceJourInterdit(i) <== abs(y_bd(i) - y_bdInit)
    }

    var objViolation = c.violation * ((S0.length * (instance.DATA.nr_slots_per_day * instance.DATA.nr_days_per_week * instance.DATA.nr_weeks)) - 1)
    if(maxSession > 0) {
      objViolation *= maxSession
    }

    val objJourInterdit = sum(distanceJourInterdit)

    val obj = Objective(objViolation + objJourInterdit)
    //val obj = Objective(objViolation)
    s.close()

    println("Objectif violation: " + objViolation.value + " / Objectif jour interdit: " + objJourInterdit.value)
    println("Objectif: "+obj.value + "\n")

    var it: Int = 0
    val MaxIt: Int = 10

    while (obj.value > 0 && it < MaxIt) {
      println(c.violation)
//      val se = it % instance.DATA.nr_sessions
      val se = S0(it % S0.length)

//      var bestOccurenceWeekday = selectMin(H_wd)(weekday => obj.assignVal(x_bwd(se), weekday))
      var (bestOccurenceWeekday, bestOccurenceDailyslot) = selectMin(H_wd, H_dh)((weekday, dailyslot) => obj.assignVal(List((x_bwd(se), weekday), (x_bdh(se), dailyslot))), (weekday, dailyslot) => true)
//      var (bestOccurenceWeek, bestOccurenceWeekday) = selectMin(H_w,H_wd)((week, weekday) => obj.assignVal(List((x_bw(se), week), (x_bwd(se), weekday))), (week, weekday) => true)

//      x_bw(se) := bestOccurenceWeek
      x_bwd(se) := bestOccurenceWeekday
      x_bdh(se) := bestOccurenceDailyslot

      println("Objectif violation: " + objViolation.value + " / Objectif jour interdit: " + objJourInterdit)
      println("Objectif: " + obj.value)

      println()

      it += 1
    }

    println("\nObjectif violation: " + objViolation.value + " / Objectif jour interdit: " + objJourInterdit)
    println("Objectif: " + obj.value)

    println()

    println()
    println("Le nombre de violation est de: " + c.violation.value)
    println("run time: " + getWatchString)

    // on retourne l'horaire, le jour et la semaine de démarage
    (x_bdh,x_bwd,x_bw, c.violation.value)

  }

  def adjacent_rooms(constraint: constraint, instance: InstanceUTP): Unit = {

  }

  def allowed_slots(c: ConstraintSystem, constraint: constraint, instance: InstanceUTP, y_bdh: Array[CBLSIntVar], y_bwd: Array[CBLSIntVar], y_bw: Array[CBLSIntVar], y_bh: Array[CBLSIntVar], y_bwh: Array[CBLSIntVar]): Unit = {
    for (i <- constraint.sessions(0).set) {
      if (constraint.parameters.length > 2) {
        instance.RULES.parameter_value(constraint.parameters(2) - 1)(0).toString match {
          case "\"day\"" => c.post(GE(y_bdh(i - 1), instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt))
            c.post(LE(y_bdh(i - 1), instance.RULES.parameter_value(constraint.parameters(1) - 1)(0).toString.toInt))
          case "\"week\"" => c.post(GE(y_bwh(i - 1), instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt))
            c.post(LE(y_bwh(i - 1), instance.RULES.parameter_value(constraint.parameters(1) - 1)(0).toString.toInt))
          case "\"global\"" => c.post(GE(y_bh(i - 1), instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt))
            c.post(LE(y_bh(i - 1), instance.RULES.parameter_value(constraint.parameters(1) - 1)(0).toString.toInt))
          case _ =>
        }
      }
      else {
        c.post(GE(y_bh(i - 1), instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt))
        c.post(LE(y_bh(i - 1), instance.RULES.parameter_value(constraint.parameters(1) - 1)(0).toString.toInt))
      }
    }
  }

  def allowed_rooms(constraint: constraint, instance: InstanceUTP): Unit = {

  }

  def assign_rooms(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i <- constraint.sessions(0).set) {
//      c.post(instance.SOLUTION.SESSIONS.session_rooms(i - 1).set(0) === instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt)
    }
  }

  def assign_slot(c: ConstraintSystem, constraint: constraint, instance: InstanceUTP, y_bh: Array[CBLSIntVar], y_bdh: Array[CBLSIntVar], y_bwd: Array[CBLSIntVar], y_bw: Array[CBLSIntVar]): Unit = {
    for (i <- constraint.sessions(0).set) {
      c.post(y_bh(i-1) === instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt)
    }
  }

  def assign_teachers(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i <- constraint.sessions(0).set) {
//      c.post(instance.SOLUTION.SESSIONS.session_teachers(i - 1).set(0) === instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt)
    }
  }

  def different_daily_slot(c: ConstraintSystem, constraint: constraint, instance: InstanceUTP, y_bdh: Array[CBLSIntVar]): Unit = {
    c.post(AllDiff(Array.tabulate(constraint.sessions(0).set.length)(se => y_bdh(constraint.sessions(0).set(se) - 1))))
  }

  def different_day(c: ConstraintSystem, constraint: constraint, instance: InstanceUTP, y_bd: Array[CBLSIntVar]): Unit = {
    c.post(AllDiff(Array.tabulate(constraint.sessions(0).set.length)(se => y_bd(constraint.sessions(0).set(se) - 1))))
  }

  def different_rooms(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- constraint.sessions(0).set) {
      for (i2 <- constraint.sessions(0).set) {
        if (i1 != i2) {
//          c.post(instance.SOLUTION.SESSIONS.session_rooms(i1 - 1).set(0) !== instance.SOLUTION.SESSIONS.session_rooms(i2 - 1).set(0))
        }
      }
    }
  }

  def different_slot(c: ConstraintSystem, constraint: constraint, instance: InstanceUTP, y_bh: Array[CBLSIntVar]): Unit = {
    c.post(AllDiff(Array.tabulate(constraint.sessions(0).set.length)(se => y_bh(constraint.sessions(0).set(se) - 1))))
  }

  def different_teachers(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- constraint.sessions(0).set) {
      for (i2 <- constraint.sessions(0).set) {
        if (i1 != i2) { // améliorer
//          c.post(NE(CBLSIntVar(instance.SOLUTION.SESSIONS.session_teachers(i1 - 1).set(0)), CBLSIntVar(instance.SOLUTION.SESSIONS.session_teachers(i2 - 1).set(0))))
        }
      }
    }
  }

  def different_week(c: ConstraintSystem, constraint: constraint, instance: InstanceUTP, y_bw: Array[CBLSIntVar]): Unit = {
    c.post(AllDiff(Array.tabulate(constraint.sessions(0).set.length)(se => y_bw(constraint.sessions(0).set(se) - 1))))
  }

  def different_weekday(c: ConstraintSystem, constraint: constraint, instance: InstanceUTP, y_bwd: Array[CBLSIntVar]): Unit = {
    c.post(AllDiff(Array.tabulate(constraint.sessions(0).set.length)(se => y_bwd(constraint.sessions(0).set(se) - 1))))
  }

  def different_weeklyslot(c: ConstraintSystem, constraint: constraint, instance: InstanceUTP, y_bwh: Array[CBLSIntVar]): Unit = {
    c.post(AllDiff(Array.tabulate(constraint.sessions(0).set.length)(se => y_bwh(constraint.sessions(0).set(se) - 1))))
  }

  def forbidden_rooms(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i <- constraint.sessions(0).set) {
      for (j <- instance.RULES.parameter_value(constraint.parameters(0) - 1)) {
//        c.post(NE(CBLSIntVar(instance.SOLUTION.SESSIONS.session_rooms(i - 1).set(0)), CBLSIntVar(j.toString.toInt)))
      }
    }
  }

  def forbidden_slots(c: ConstraintSystem, s: Store, constraint: constraint, instance: InstanceUTP, class_part:Array[Int], y_bdh: Array[CBLSIntVar], y_bwd: Array[CBLSIntVar], y_bw: Array[CBLSIntVar], y_bh: Array[CBLSIntVar], y_bwh: Array[CBLSIntVar]): Unit = {
    for (i <- constraint.sessions(0).set) {
      if (constraint.parameters.length > 2) {
        instance.RULES.parameter_value(constraint.parameters(2) - 1)(0).toString match {
          case "\"day\"" => var tmp: Array[Int] = Array()
            for (h <- instance.DATA.part_dailyslots(class_part(instance.SOLUTION.SESSIONS.session_class(i - 1) - 1) - 1).set) {
              if (h < instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt || h > instance.RULES.parameter_value(constraint.parameters(1) - 1)(0).toString.toInt) {
                tmp :+= h
              }
            }
            c.post(BelongsToConst(y_bdh(i - 1), tmp.toSet))
          case "\"week\"" => var tmp: Array[Int] = Array()
            for (d <- instance.DATA.part_days(class_part(instance.SOLUTION.SESSIONS.session_class(i - 1) - 1) - 1).set) {
              for (h <- instance.DATA.part_dailyslots(class_part(instance.SOLUTION.SESSIONS.session_class(i - 1) - 1) - 1).set) {
                val tmp2 = (d - 1) * instance.DATA.nr_slots_per_day + h
                if (tmp2 < instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt || tmp2 > instance.RULES.parameter_value(constraint.parameters(1) - 1)(0).toString.toInt) {
                  tmp :+= tmp2
                }
              }
            }
            c.post(BelongsToConst(y_bwh(i - 1), tmp.toSet))
          case "\"global\"" => var tmp: Array[Int] = Array()
            for (w <- instance.DATA.part_weeks(class_part(instance.SOLUTION.SESSIONS.session_class(i - 1) - 1) - 1).set) {
              for (d <- instance.DATA.part_days(class_part(instance.SOLUTION.SESSIONS.session_class(i - 1) - 1) - 1).set) {
                for (h <- instance.DATA.part_dailyslots(class_part(instance.SOLUTION.SESSIONS.session_class(i - 1) - 1) - 1).set) {
                  val tmp2 = (w - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day + (d - 1) * instance.DATA.nr_slots_per_day + h
                  if (tmp2 < instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt || tmp2 > instance.RULES.parameter_value(constraint.parameters(1) - 1)(0).toString.toInt) {
                    tmp :+= tmp2
                  }
                }
              }
            }
            c.post(BelongsToConst(y_bh(i - 1), tmp.toSet))
          case _ =>
        }
      }
      else {
        var tmp: Array[Int] = Array()
        for(w <- instance.DATA.part_weeks(class_part(instance.SOLUTION.SESSIONS.session_class(i-1) - 1) - 1).set){
          for(d <- instance.DATA.part_days(class_part(instance.SOLUTION.SESSIONS.session_class(i-1) - 1) - 1).set){
            for(h <- instance.DATA.part_dailyslots(class_part(instance.SOLUTION.SESSIONS.session_class(i-1) - 1) - 1).set){
              val tmp2 = (w - 1) * instance.DATA.nr_days_per_week * instance.DATA.nr_slots_per_day + (d - 1) * instance.DATA.nr_slots_per_day + h
              if(tmp2 < instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt || tmp2 > instance.RULES.parameter_value(constraint.parameters(1) - 1)(0).toString.toInt){
                tmp :+= tmp2
              }
            }
          }
        }
        c.post(BelongsToConst(y_bh(i-1), tmp.toSet))
      }
    }
  }

  def forbidden_teachers(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i <- constraint.sessions(0).set) {
      for (j <- instance.RULES.parameter_value(constraint.parameters(0) - 1)) {
//        c.post(NE(CBLSIntVar(instance.SOLUTION.SESSIONS.session_teachers(i - 1).set(0)), CBLSIntVar(j.toString.toInt)))
      }
    }
  }

  def no_overlap(c: ConstraintSystem, constraint: constraint, instance: InstanceUTP, class_part:Array[Int], y_bh: Array[CBLSIntVar]): Unit = {
    var sessions: Array[Int] = conditionnalite(constraint._type(0), constraint, instance)

    for(i <- 0 until sessions.length - 1){
      for(j <- i+1 until sessions.length){
          c.post(Disjunctive(
          Array(y_bh(constraint.sessions(0).set(i)-1), y_bh(constraint.sessions(0).set(j)-1)),
          Array(instance.DATA.part_session_length(class_part(instance.SOLUTION.SESSIONS.session_class(constraint.sessions(0).set(i)-1) - 1) - 1), instance.DATA.part_session_length(class_part(instance.SOLUTION.SESSIONS.session_class(constraint.sessions(0).set(j)-1) - 1) - 1))
          )
        )
      }
    }
  }

  def pairwise_no_overlap(constraint: constraint, instance: InstanceUTP): Unit = {

  }

//  def periodic(constraint: constraint, instance: InstanceUTP, x_dailyslot: Array[CBLSIntVar], x_weekday: Array[CBLSIntVar], x_week: Array[CBLSIntVar]): Unit = {
//    var value: Int = instance.RULES.parameter_value(constraint.parameters(0) - 1)(0).toString.toInt
//    var unit: String = instance.RULES.parameter_value(constraint.parameters(1) - 1)(0).toString
//
//    unit match {
//      case "\"day\"" => // à revoir
//        for (i <- 0 until constraint.sessions(0).set.length - 1) {
//          c.post(EQ(x_weekday(constraint.sessions(0).set(i)-1) + value, x_weekday(constraint.sessions(0).set(i+1)-1)))
//          c.post(EQ(x_dailyslot(constraint.sessions(0).set(i)-1), x_dailyslot(constraint.sessions(0).set(i+1)-1)))
//        }
//      case "\"week\"" =>
//        for (i <- 0 until constraint.sessions(0).set.length - 1) {
//          c.post(EQ(x_week(constraint.sessions(0).set(i)-1) + value, x_week(constraint.sessions(0).set(i+1)-1)))
//        }
//      case _ =>
//
//    }
//  }

  def required_rooms(constraint: constraint, instance: InstanceUTP): Unit = {

  }

  def same_daily_slot(c: ConstraintSystem, constraint: constraint, instance: InstanceUTP, S0: Array[Int], y_bdh: Array[CBLSIntVar]): Unit = {
    for (i1 <- 0 until constraint.sessions(0).set.length - 1) {
      for (i2 <- i1 + 1 until constraint.sessions(0).set.length) {
        if(!(S0.contains(constraint.sessions(0).set(i1) - 1) || S0.contains(constraint.sessions(0).set(i2) - 1))) {
          c.post(y_bdh(constraint.sessions(0).set(i1) - 1) === y_bdh(constraint.sessions(0).set(i2) - 1))
        }
      }
    }
  }

  def same_day(c: ConstraintSystem, constraint: constraint, instance: InstanceUTP, S0: Array[Int], y_bd: Array[CBLSIntVar]): Unit = {
    for (i1 <- 0 until constraint.sessions(0).set.length - 1) {
      for (i2 <- i1 + 1 until constraint.sessions(0).set.length) {
        if(!(S0.contains(constraint.sessions(0).set(i1) - 1) || S0.contains(constraint.sessions(0).set(i2) - 1))) {
          c.post(y_bd(constraint.sessions(0).set(i1) - 1) === y_bd(constraint.sessions(0).set(i2) - 1))
        }
      }
    }
  }

  def same_rooms(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- constraint.sessions(0).set) {
      for (i2 <- constraint.sessions(0).set) {
        if (i1 != i2) {
//          c.post(EQ(CBLSIntVar(instance.SOLUTION.SESSIONS.session_rooms(i1 - 1).set(0)), CBLSIntVar(instance.SOLUTION.SESSIONS.session_rooms(i2 - 1).set(0))))
        }
      }
    }
  }

  def same_slot(c: ConstraintSystem, constraint: constraint, instance: InstanceUTP, S0: Array[Int], y_bh:Array[CBLSIntVar]): Unit = {
    for (i1 <- 0 until constraint.sessions(0).set.length - 1) {
      for (i2 <- i1 + 1 until constraint.sessions(0).set.length) {
        if(!(S0.contains(constraint.sessions(0).set(i1) - 1) || S0.contains(constraint.sessions(0).set(i2) - 1))) {
          c.post(y_bh(constraint.sessions(0).set(i1) - 1) === y_bh(constraint.sessions(0).set(i2) - 1))
        }
      }
    }
  }

  def same_teachers(constraint: constraint, instance: InstanceUTP): Unit = {
    for (i1 <- constraint.sessions(0).set) {
      for (i2 <- constraint.sessions(0).set) {
        if (i1 != i2) { // améliorer
//          c.post(EQ(CBLSIntVar(instance.SOLUTION.SESSIONS.session_teachers(i1 - 1).set(0)), CBLSIntVar(instance.SOLUTION.SESSIONS.session_teachers(i2 - 1).set(0))))
        }
      }
    }
  }

  def same_week(c: ConstraintSystem, constraint: constraint, instance: InstanceUTP, S0: Array[Int], y_bw: Array[CBLSIntVar]): Unit = {
    for (i1 <- 0 until constraint.sessions(0).set.length - 1) {
      for (i2 <- i1 + 1 until constraint.sessions(0).set.length) {
        if(!(S0.contains(constraint.sessions(0).set(i1) - 1) || S0.contains(constraint.sessions(0).set(i2) - 1))) {
          c.post(y_bw(constraint.sessions(0).set(i1) - 1) === y_bw(constraint.sessions(0).set(i2) - 1))
        }
      }
    }
  }

  def same_weekday(c: ConstraintSystem, constraint: constraint, instance: InstanceUTP, S0: Array[Int], y_bwd: Array[CBLSIntVar]): Unit = {
    for (i1 <- 0 until constraint.sessions(0).set.length - 1) {
      for (i2 <- i1 + 1 until constraint.sessions(0).set.length) {
        if(!(S0.contains(constraint.sessions(0).set(i1) - 1) || S0.contains(constraint.sessions(0).set(i2) - 1))) {
          c.post(y_bwd(constraint.sessions(0).set(i1) - 1) === y_bwd(constraint.sessions(0).set(i2) - 1))
        }
      }
    }
  }

  def same_weeklyslot(c: ConstraintSystem, constraint: constraint, instance: InstanceUTP, S0: Array[Int], y_bwh: Array[CBLSIntVar]): Unit = {
    for (i1 <- 0 until constraint.sessions(0).set.length - 1) {
      for (i2 <- i1 + 1 until constraint.sessions(0).set.length) {
        if(!(S0.contains(constraint.sessions(0).set(i1) - 1) || S0.contains(constraint.sessions(0).set(i2) - 1))) {
          c.post(y_bwh(constraint.sessions(0).set(i1) - 1) === y_bwh(constraint.sessions(0).set(i2) - 1))
        }
      }
    }
  }

  def sequenced(c: ConstraintSystem, constraint: constraint, instance: InstanceUTP, S0: Array[Int], y_bh: Array[CBLSIntVar], y_eh: Array[CBLSIntVar]): Unit = {
    if (constraint.sessions.length > 1) {
      for (se1 <- constraint.sessions(0).set) {
        for (se2 <- constraint.sessions(1).set) {
          c.post(LE(y_eh(se1 - 1), y_bh(se2 - 1)))
        }
      }
    }
    else{
      for (i1 <- 0 until constraint.sessions(0).set.length - 1) {
        for (i2 <- i1 + 1 until constraint.sessions(0).set.length) {
          c.post(LE(y_eh(constraint.sessions(0).set(i1) - 1), y_bh(constraint.sessions(0).set(i2) - 1)))
        }
      }
    }
  }

    def conditionnalite(_type: String, constraint: constraint, instance: InstanceUTP): Array[Int] = {
      var sessions: Array[Int] = Array()

      _type match {
        case "lecturer" =>
          for (i <- constraint.sessions(0).set) {
            for (t <- instance.SOLUTION.SESSIONS.session_teachers(i - 1).set) {
              if (constraint.elements(0) == t) {
                sessions :+= i
              }
            }
          }
        case "room" =>
          for (i <- constraint.sessions(0).set) {
            for (r <- instance.SOLUTION.SESSIONS.session_rooms(i - 1).set) {
              if (constraint.elements(0) == r) {
                sessions :+= i
              }
            }
          }
        case _ => sessions = constraint.sessions(0).set.toArray
      }

      (sessions)
    }

  def write_solution_file(instance: InstanceUTP, x_bdh: Array[CBLSIntVar] = null, x_bwd: Array[CBLSIntVar] = null, x_bw: Array[CBLSIntVar] = null, fileName: String, original: Boolean): Unit = {
    var out: String = ""

    val lines = Source.fromFile(fileName).getLines().toList

    var i: Int = 0

    while (lines(i).trim() != "<sessions>" && i < lines.length - 1) {
      out += lines(i) + "\n"
      i += 1
    }

    out += print_xml(instance, x_bdh,x_bwd,x_bw, original)
    out += "\n</solution>\n</timetabling>"

    var fileName2 = fileName.substring(0, fileName.length - 4)
    if(original) {
      fileName2 += "_original.xml"
    }
    else {
      fileName2 += "_modifie.xml"
    }

    val file = new File(fileName2)
    val bw = new BufferedWriter(new FileWriter(file))

    bw.write(out)

    bw.close()
  }

  def print_xml(instance: InstanceUTP, x_bdh: Array[CBLSIntVar], x_bwd: Array[CBLSIntVar] , x_bw: Array[CBLSIntVar], original: Boolean): String = {
    var out: String = "<classes>\n"

    for (i <- 0 until instance.DATA.nr_classes) {
      out += "<class refId = \"" + instance.DATA.class_name(i) + "\">\n"
      out += "    <groups>\n" + print_xml_group_solution(instance, instance.SOLUTION.SESSIONS.session_class(i) - 1) + "    </groups>\n"
      out += "    <teachers>\n" + print_xml_teacher_solution(instance, instance.SOLUTION.SESSIONS.session_class(i) - 1) + "    </teachers>\n"
      out += "    <rooms>\n" + print_xml_room_solution(instance, instance.SOLUTION.SESSIONS.session_class(i) - 1) + "    </rooms>\n"
      out += "</class>\n"
    }

    out += "</classes>\n<sessions>\n"

    if(original) {
      for (i <- 0 until instance.DATA.nr_sessions) {
        out += "<session rank= \"" + instance.SOLUTION.SESSIONS.session_rank(i) + "\" class=\"" + instance.DATA.class_name(instance.SOLUTION.SESSIONS.session_class(i) - 1) + "\">\n"
        out += "    <startingSlot dailySlot=\"" + instance.SOLUTION.SESSIONS.session_dailyslot(i) + "\" day=\"" + instance.SOLUTION.SESSIONS.session_day(i) + "\" week=\"" + instance.SOLUTION.SESSIONS.session_week(i) + "\" />\n"
        out += "    <rooms>\n" + print_xml_room_solution(instance, i) + "    </rooms>\n"
        out += "    <teachers>\n" + print_xml_teacher_solution(instance, i) + "    </teachers>\n"
        out += "</session>\n"
      }
    }
    else{
      for (i <- 0 until instance.DATA.nr_sessions) {
        out += "<session rank= \"" + instance.SOLUTION.SESSIONS.session_rank(i) + "\" class=\"" + instance.DATA.class_name(instance.SOLUTION.SESSIONS.session_class(i) - 1) + "\">\n"
        out += "    <startingSlot dailySlot=\"" + x_bdh(i).value + "\" day=\"" + x_bwd(i).value + "\" week=\"" + x_bw(i).value + "\" />\n"
        out += "    <rooms>\n" + print_xml_room_solution(instance, i) + "    </rooms>\n"
        out += "    <teachers>\n" + print_xml_teacher_solution(instance, i) + "    </teachers>\n"
        out += "</session>\n"
      }
    }

    out += "</sessions>\n"

    out
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

  def class_part(instance: InstanceUTP): Array[Int] = {
    var class_part: Array[Int] = new Array[Int](instance.DATA.nr_classes)
    var i = 0

    for(p <- 0 until instance.DATA.nr_parts){
      for(c <- 0 until instance.DATA.part_classes(p).set.length){
        class_part(i) = p + 1
        i += 1
      }
    }

    (class_part)
  }
}
