import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CheckoutSpec extends AnyWordSpec with Matchers {

  "Checkout Service" should {

    "get correct price for a single apple" in {
      CheckoutService.checkout(List("APPLE")) shouldBe 60
    }

    "get correct price for a single Orange" in {
      CheckoutService.checkout(List("ORANGE")) shouldBe 25
    }

    "return nothing for unrecognised items" in {
      CheckoutService.checkout(List("BLAH")) shouldBe 0
    }

    "get correct totals for one item of each" in {
    CheckoutService.checkout(List("APPLE", "ORANGE")) shouldBe 85
    }

    "Apply the Deal for apple" in {
      CheckoutService.checkout(List("APPLE", "APPLE")) shouldBe 60
    }

    "Apply the Deal for Oranges" in {
      CheckoutService.checkout(List("ORANGE", "ORANGE", "ORANGE")) shouldBe 50
    }

    "Apply the Deal for apples, but full price for 3rd" in {
      CheckoutService.checkout(List("APPLE", "APPLE", "APPLE")) shouldBe 120
    }

    "Apply the Deal for apples, but full price for 4th" in {
      CheckoutService.checkout(List("ORANGE", "ORANGE", "ORANGE", "ORANGE")) shouldBe 75
    }
  }


}
