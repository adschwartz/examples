package api

import io.circe.generic.extras.ConfiguredJsonCodec
import cats.data.ValidatedNel
import cats.implicits._
import cats.implicits.{catsSyntaxTuple2Semigroupal, catsSyntaxValidatedId}


object CirceValidation {

  import io.circe.generic.extras.Configuration

  implicit val defaults: Configuration = Configuration.default.withDefaults

  @ConfiguredJsonCodec case class Car(
    brand: String,
    wheelCount: Int,
    body: Body,
  )

  @ConfiguredJsonCodec case class Body(
    paintColor: String,
    doors: List[Door],
  )

  @ConfiguredJsonCodec case class Door(
    tinted: Boolean,
    doorHandle: DoorHandle,
  )

  @ConfiguredJsonCodec case class DoorHandle(
    numberOfParts: Int,
  )

  object CarValidator {
    def validate(car: Car): ValidatedNel[String, Car] = (
      if (Option(car.brand).exists(_.nonEmpty)) car.brand.validNel
      else "Brand must be provided".invalidNel,
      if (car.wheelCount < 2 || car.wheelCount > 6) "A car must have been 2 and 6 wheels".invalidNel
      else car.wheelCount.validNel,
      BodyValidator.validate(car.body),
      ).mapN[Car](Car.apply _)
  }

  object BodyValidator {
    def validate(body: Body): ValidatedNel[String, Body] = (
      if (Option(body.paintColor).exists(_.nonEmpty)) body.paintColor.validNel
      else "Body paint color must be provided".invalidNel,
      body.doors.map(DoorValidator.validate).sequence,
      ).mapN[Body](Body.apply _)
  }

  object DoorValidator {
    def validate(door: Door): ValidatedNel[String, Door] = (
      door.tinted.validNel,
      DoorHandleValidator.validate(door.doorHandle),
      ).mapN[Door](Door.apply _)
  }

  object DoorHandleValidator {
    def validate(doorHandle: DoorHandle): ValidatedNel[String, DoorHandle] = (
      if (doorHandle.numberOfParts > 0) doorHandle.numberOfParts.validNel
      else "The number of door handle parts must be greater than 0".invalidNel
      ).map[DoorHandle](DoorHandle.apply _)

  }


}
