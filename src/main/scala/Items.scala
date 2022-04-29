import scala.math._


sealed trait Items {
  def totalPrice(num: Int): Int
}

object Items {
  case object Apple extends Items {
    def totalPrice(num: Int): Int = (floor(num / 2) * 60).toInt + ((num % 2) * 60)
  }
  case object Orange extends Items {
    def totalPrice(num: Int): Int = (floor(num * 2/3) * 25).toInt  + (num % 3) * 25
  }

  def fromString(input: String): Option[Items] = {
      input.toUpperCase match {
      case "APPLE"  => Some(Apple)
      case "ORANGE" => Some(Orange)
      case _        => None
    }
  }
}