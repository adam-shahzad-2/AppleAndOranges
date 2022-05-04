import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DateFormatterSpec extends AnyWordSpec with Matchers {

  "Date Formatter" should {

    "return expected output when only valid response" in {
      DateFormatter.input("10/31/2001") shouldBe "2001-10-31"
    }

    "return expected output when multiple segments can refer to different time attributes" in {
      DateFormatter.input("10/11/12") shouldBe "2012-10-11"
    }

    "return expected output when every number is the same" in {
      DateFormatter.input("10/10/10") shouldBe "10-10-10"
    }

    "return invalid response when dates are invalid" in {
      DateFormatter.input("100/100/100") shouldBe "INVALID DATE"
    }
  }

  "yearFormatter" should {
    "add 2000 if the year contains two digits" in {
      DateFormatter.formatYear("20") shouldBe "2020"
    }
  }

}
