object CheckoutService {

  def checkout(basket: List[String]): Int = {
    basket.flatMap(entry => Items.fromString(entry)).groupBy(identity).view.mapValues(_.size).map{itemTotalTuple =>
      itemTotalTuple._1.totalPrice(itemTotalTuple._2)
    }.sum
  }
}
