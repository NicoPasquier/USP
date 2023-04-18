// library spray-json: https://github.com/spray/spray-json
import spray.json._
import DefaultJsonProtocol._


case class set(set: List[Int])

case class data(
                 nr_weeks: Int,
                 nr_days_per_week: Int,
                 nr_slots_per_day: Int,
                 nr_courses:Int,
                 nr_parts: Int,
                 course_parts: Seq[set],
                 nr_classes: Int,
                 part_classes: Seq[set],
                 nr_sessions: Int,
                 max_part_sessions: Int,
                 part_nr_sessions: Seq[Int],
                 nr_equipments: Int,
                 nr_rooms: Int,
                 nr_teachers: Int,
                 nr_students: Int,
                 nr_part_rooms: Int,
                 part_rooms: Seq[set],
                 nr_part_teachers: Int,
                 part_teachers: Seq[set],
                 part_dailyslots: Seq[set],
                 part_days: Seq[set],
                 part_weeks: Seq[set],
                 part_session_lengt: Seq[Int],
                 max_equipment_count: Int,
                 max_class_maxheadcount: Int,
                 max_teacher_session: Int,
                 max_teacher_sessions: Int,
                 equipment_count: Seq[Int],
                 max_room_capacity: Int,
                 room_capacity: Seq[Int],
                 part_room_use: Seq[String],
                 nr_part_room_mandatory: Int,
                 part_room_mandatory: Seq[Seq[Int]],
                 part_teacher_sessions_count: Seq[Seq[Int]],
                 part_session_teacher_count: Seq[Int],
                 class_maxheadcount: Seq[Int],
                 class_parent: Seq[Int],
                 student_courses: Seq[set],
                 equipment_name: Seq[String],
                 room_name: Seq[String],
                 teacher_name: Seq[String],
                 student_name: Seq[String],
                 course_name: Seq[String],
                 part_name: Seq[String],
                 class_name: Seq[String],
                 nr_labels: Int,
                 label_name: Seq[String],
                 room_label: Seq[set],
                 teacher_label: Seq[set],
                 student_label: Seq[set],
                 course_label: Seq[set],
                 part_label: Seq[set]
               )

case class rules(
                  nr_rules: Int,
                  nr_scopes: Int,
                  rule_scopes: Seq[set],
                  scope_type: Seq[String],
                  mask_length: Int,
                  scope_mask: Seq[set],
                  nr_filters: Int,
                  scope_filters: Seq[set],
                  filter_type: Seq[String],
                  filter_elements: Seq[set],
                  rule_constraint: Seq[String],
                  constraint_hardness: Seq[String],
                  nr_parameters: Int,
                  constraint_parameters: Seq[set],
                  parameter_name: Seq[String],
                  parameter_type: Seq[String],
                  max_parameter_value: Int,
                  max_parameter_size: Int,
                  parameter_value: Seq[Seq[Any]]
                )

case class groups(
                   nr_groups: Int,
                   max_group_headcount: Int,
                   group_headcount: Seq[Int],
                   group_name: Seq[String],
                   group_students: Seq[set],
                   group_classes: Seq[set],
                   group_sessions: Seq[set]
                 )

case class _class(
                   class_teachers: Seq[Int],
                   class_rooms: Seq[Int],
                   class_groups: Seq[Seq[set]]
                  )

case class sessions(
                     session_rank: Seq[Int],
                     session_class: Seq[Int],
                     session_dailyslot: Seq[Int],
                     session_day: Seq[Int],
                     session_week: Seq[Int],
                     session_rooms: Seq[set],
                     session_teachers: Seq[set]
                   )

case class solution(
                     GROUPS: groups,
                     CLASS: _class,
                     SESSIONS: sessions
                   )

case class constraint(
                       rule: Int,
                       constraint: String,
                       hardness: String,
                       arity: Int,
                       _type: Seq[String],
                       elements: Seq[Int],
                       sessions: Seq[set],
                       parameters: Seq[Int]
                     )

case class InstanceUTP (
                         DATA: data,
                         RULES: rules,
                         SOLUTION: solution,
                         CONSTRAINTS: Seq[constraint]
                       )

object MyJsonProtocol extends DefaultJsonProtocol {

  implicit val setFormat: RootJsonFormat[set] = jsonFormat1(set)

  implicit object dataJsonFormat extends RootJsonFormat[data] {
    def write(c: data) = JsObject(
      "nr_weeks" -> JsNumber(c.nr_weeks)
    )

    def read(json: JsValue): data = {
      val fields = json.asJsObject.fields
      data(
        fields("nr_weeks").convertTo[Int],
        fields("nr_days_per_week").convertTo[Int],
        fields("nr_slots_per_day").convertTo[Int],
        fields("nr_courses").convertTo[Int],
        fields("nr_parts").convertTo[Int],
        fields("course_parts").convertTo[Seq[set]],
        fields("nr_classes").convertTo[Int],
        fields("part_classes").convertTo[Seq[set]],
        fields("nr_sessions").convertTo[Int],
        fields("max_part_sessions").convertTo[Int],
        fields("part_nr_sessions").convertTo[Seq[Int]],
        fields("nr_equipments").convertTo[Int],
        fields("nr_rooms").convertTo[Int],
        fields("nr_teachers").convertTo[Int],
        fields("nr_students").convertTo[Int],
        fields("nr_part_rooms").convertTo[Int],
        fields("part_rooms").convertTo[List[set]],
        fields("nr_part_teachers").convertTo[Int],
        fields("part_teachers").convertTo[Seq[set]],
        fields("part_dailyslots").convertTo[Seq[set]],
        fields("part_days").convertTo[Seq[set]],
        fields("part_weeks").convertTo[Seq[set]],
        fields("part_session_length").convertTo[Seq[Int]],
        fields("max_equipment_count").convertTo[Int],
        fields("max_class_maxheadcount").convertTo[Int],
        fields("max_teacher_session").convertTo[Int],
        fields("max_teacher_sessions").convertTo[Int],
        fields("equipment_count").convertTo[Seq[Int]],
        fields("max_room_capacity").convertTo[Int],
        fields("room_capacity").convertTo[Seq[Int]],
        fields("part_room_use").convertTo[Seq[String]],
        fields("nr_part_room_mandatory").convertTo[Int],
        fields("part_room_mandatory").convertTo[Seq[Seq[Int]]],
        fields("part_teacher_sessions_count").convertTo[Seq[Seq[Int]]],
        fields("part_session_teacher_count").convertTo[Seq[Int]],
        fields("class_maxheadcount").convertTo[Seq[Int]],
        fields("class_parent").convertTo[Seq[Int]],
        fields("student_courses").convertTo[Seq[set]],
        fields("equipment_name").convertTo[Seq[String]],
        fields("room_name").convertTo[Seq[String]],
        fields("teacher_name").convertTo[Seq[String]],
        fields("student_name").convertTo[Seq[String]],
        fields("course_name").convertTo[Seq[String]],
        fields("part_name").convertTo[Seq[String]],
        fields("class_name").convertTo[Seq[String]],
        fields("nr_labels").convertTo[Int],
        fields("label_name").convertTo[Seq[String]],
        fields("room_label").convertTo[Seq[set]],
        fields("teacher_label").convertTo[Seq[set]],
        fields("student_label").convertTo[Seq[set]],
        fields("course_label").convertTo[Seq[set]],
        fields("part_label").convertTo[Seq[set]]
      )
    }
  }

  implicit object rulesJsonFormat extends RootJsonFormat[rules] {
    def write(c: rules) = JsObject(

    )

    def read(json: JsValue): rules = {
      val fields = json.asJsObject.fields
      rules(
        fields("nr_rules").convertTo[Int],
        fields("nr_scopes").convertTo[Int],
        fields("rule_scopes").convertTo[Seq[set]],
        fields("scope_type").convertTo[Seq[String]],
        fields("mask_length").convertTo[Int],
        fields("scope_mask").convertTo[Seq[set]],
        fields("nr_filters").convertTo[Int],
        fields("scope_filters").convertTo[Seq[set]],
        fields("filter_type").convertTo[Seq[String]],
        fields("filter_elements").convertTo[Seq[set]],
        fields("rule_constraint").convertTo[Seq[String]],
        fields("constraint_hardness").convertTo[Seq[String]],
        fields("nr_parameters").convertTo[Int],
        fields("constraint_parameters").convertTo[Seq[set]],
        fields("parameter_name").convertTo[Seq[String]],
        fields("parameter_type").convertTo[Seq[String]],
        fields("max_paramater_value").convertTo[Int],
        fields("max_parameter_size").convertTo[Int],
        fields("parameter_value").convertTo[Seq[Seq[JsValue]]]
      )
    }
  }

  implicit object groupsJsonFormat extends RootJsonFormat[groups] {
    def write(c: groups) = JsObject(

    )

    def read(json: JsValue): groups = {
      val fields = json.asJsObject.fields
      groups(
        fields("nr_groups").convertTo[Int],
        fields("max_group_headcount").convertTo[Int],
        fields("group_headcount").convertTo[Seq[Int]],
        fields("group_name").convertTo[Seq[String]],
        fields("group_students").convertTo[Seq[set]],
        fields("group_classes").convertTo[Seq[set]],
        fields("group_sessions").convertTo[Seq[set]]
      )
    }
  }

  implicit object classJsonFormat extends RootJsonFormat[_class] {
    def write(c: _class) = JsObject(

    )

    def read(json: JsValue): _class = {
      val fields = json.asJsObject.fields
      _class(
        fields("class_teachers").convertTo[Seq[Int]],
        fields("class_rooms").convertTo[Seq[Int]],
        fields("class_groups").convertTo[Seq[Seq[set]]]
      )
    }
  }

  implicit object sessionsJsonFormat extends RootJsonFormat[sessions] {
    def write(c: sessions) = JsObject(

    )

    def read(json: JsValue): sessions = {
      val fields = json.asJsObject.fields
      sessions(
        fields("session_rank").convertTo[Seq[Int]],
        fields("session_class").convertTo[Seq[Int]],
        fields("session_dailyslot").convertTo[Seq[Int]],
        fields("session_day").convertTo[Seq[Int]],
        fields("session_week").convertTo[Seq[Int]],
        fields("session_rooms").convertTo[Seq[set]],
        fields("session_teachers").convertTo[Seq[set]]
      )
    }
  }

  implicit object solutionJsonFormat extends RootJsonFormat[solution] {
    def write(c: solution) = JsObject(

    )

    def read(json: JsValue): solution = {
      val fields = json.asJsObject.fields
      solution(
        fields("GROUPS").convertTo[groups],
        fields("CLASS").convertTo[_class],
        fields("SESSIONS").convertTo[sessions]
      )
    }
  }

  implicit object constraintJsonFormat extends RootJsonFormat[constraint] {
    def write(c: constraint) = JsObject(

    )

    def read(json: JsValue): constraint = {
      val fields = json.asJsObject.fields
      constraint(
        fields("rule").convertTo[Int],
        fields("constraint").convertTo[String],
        fields("hardness").convertTo[String],
        fields("arity").convertTo[Int],
        fields("type").convertTo[Seq[String]],
        fields("elements").convertTo[Seq[Int]],
        fields("sessions").convertTo[Seq[set]],
        fields("parameters").convertTo[Seq[Int]]
      )
    }
  }

  implicit object instanceUTPJsonFormat extends RootJsonFormat[InstanceUTP] {
    def write(c: InstanceUTP) = JsObject(

    )

    def read(json: JsValue): InstanceUTP = {
      val fields = json.asJsObject.fields
      InstanceUTP(
        fields("DATA").convertTo[data],
        fields("RULES").convertTo[rules],
        fields("SOLUTION").convertTo[solution],
        fields("CONSTRAINTS").convertTo[Seq[constraint]]
      )
    }
  }

}