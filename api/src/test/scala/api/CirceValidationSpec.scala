package api

import api.CirceValidation._
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import io.circe
import io.circe.generic.semiauto.deriveDecoder
import io.circe.syntax.EncoderOps
import io.circe.{CursorOp, DecodingFailure, jawn}
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CirceValidationSpec extends BaseSpec {

  val doorHandle1 = DoorHandle(numberOfParts = 2)
  val doorHandle2 = DoorHandle(numberOfParts = 2)
  val door1 = Door(tinted = true, doorHandle = doorHandle1)
  val door2 = Door(tinted = true, doorHandle = doorHandle2)

  val body1 = Body(paintColor = "", doors = List(door1, door2))
  val car1 = Car(brand = "Audi", wheelCount = 8, body = body1)

  "CirceValidation" must {

    "generate json" in {
      val jsonString = (car1: Car).asJson.spaces2
      println(jsonString)
    }

    "validate json" in {
      val jsonText =
        s"""{
           |  "brand" : "Audi",
           |  "wheelCount" : 8,
           |  "body" : {
           |    "paintColor" : "",
           |    "doors" : [
           |      {
           |        "tinted" : true,
           |        "doorHandle" : {
           |          "numberOfParts" : 2
           |        }
           |      },
           |      {
           |        "tinted" : true,
           |        "doorHandle" : {
           |          "numberOfParts" : -1
           |        }
           |      }
           |    ]
           |  }
           |}""".stripMargin

      implicit val carDecoder = deriveDecoder[Car]

      val decoded: ValidatedNel[circe.Error, Car] = jawn.decodeAccumulating[Car](jsonText)
      val converted: Validated[NonEmptyList[String], Car] = decoded.leftMap { e =>
        e.map { t =>
          val decodingFailure = t.asInstanceOf[DecodingFailure]
          val path = CursorOp.opsToPath(decodingFailure.history)
          val message =  decodingFailure.message
          s"$path: $message"
        }
      }
      println(converted)
      // prints:
      // Valid(Car(Audi,8,Body(,List(Door(true,DoorHandle(2)), Door(true,DoorHandle(-1))))))
    }

  }

}
