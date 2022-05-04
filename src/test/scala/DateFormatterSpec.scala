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
      DateFormatter.input("10/10/10") shouldBe "2010-10-10"
    }

    "return invalid response when dates are invalid" in {
      DateFormatter.input("100/100/100") shouldBe "INVALID DATE"
    }
  }

  "yearFormatter" should {
    "add 2000 if the year contains two digits" in {
      DateFormatter.formatYear("22") shouldBe "2022"
    }

    "not add if the year already has four digits" in {
      DateFormatter.formatYear("2022") shouldBe "2022"
    }
  }

  "fillMaybeLists" should {
    "create the correct Potential date from 10/10/10" in {
      DateFormatter.fillMaybeLists(List(10,10,10)) shouldBe PotentialDate(
        potentialDay = List(10,10,10),
        potentialMonth = List(10,10,10),
        potentialYear = List(10,10,10))
    }

    "create the correct Potential date from 31/12/2001" in {
      DateFormatter.fillMaybeLists(List(31,12,2001)) shouldBe PotentialDate(
        potentialDay = List(31,12),
        potentialMonth = List(12),
        potentialYear = List(31,12,2001))
    }

    "create the correct Potential date invalid data -1/-1/-1" in {
      DateFormatter.fillMaybeLists(List(-1,-1,-1)) shouldBe PotentialDate(
        potentialDay = List.empty,
        potentialMonth = List.empty,
        potentialYear = List.empty)
    }
  }

}
