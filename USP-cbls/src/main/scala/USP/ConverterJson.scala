package USP

import spray.json._
import DefaultJsonProtocol._
import USP.MyJsonProtocol._

class ConverterJson (val name: String){
  var fileName: String = name

  def readJson(): InstanceUTP = {
    val source = scala.io.Source.fromFile(this.fileName)
    val lines = try source.mkString finally source.close()
    val jsonAst = lines.parseJson // or JsonParser(source)
    val instance = jsonAst.convertTo[InstanceUTP]

    //println(instance)

    return instance
  }
}
