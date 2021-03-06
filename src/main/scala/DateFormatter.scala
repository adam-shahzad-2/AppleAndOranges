object DateFormatter {

  def input(inputString: String): String = {

    val inputList: List[Int] = inputString.split('/').toList.map(_.toInt)

    handleMaybeLists(potentialDate = fillMaybeLists(inputList))

  }

  def fillMaybeLists(inputList: List[Int]
                    ): PotentialDate = {

    var maybeMonthList: List[Int] = List.empty
    var maybeDayList: List[Int] = List.empty
    var maybeYearList: List[Int] = List.empty

    maybeMonthList = inputList.flatMap{ segment =>
      Some(segment).filter(validMonth)
    }

    maybeDayList = inputList.flatMap{ segment =>
      Some(segment).filter(simpleValidDayOfMonth)
    }

    maybeYearList = inputList.flatMap{ segment =>
      Some(segment).filter(validYear)
    }

    PotentialDate(maybeDayList, maybeMonthList, maybeYearList)

  }

  private def filterFirst(seq: Seq[Int], value: Int): Seq[Int] = {
    val index = seq.indexOf(value)
    if (index < 0) {
      seq
    } else if (index == 0) {
      seq.tail
    } else {
      val (a, b) = seq.splitAt(index)
      a ++ b.tail
    }
  }


  def handleMaybeLists(potentialDate: PotentialDate): String = {

    if (potentialDate.potentialMonth.isEmpty || potentialDate.potentialDay.isEmpty) return "INVALID DATE"

    var newMaybeDayList = filterFirst(potentialDate.potentialDay, potentialDate.potentialMonth.head)
    var newMaybeYearList = filterFirst(potentialDate.potentialYear, potentialDate.potentialMonth.head)
    newMaybeYearList = filterFirst(newMaybeYearList, newMaybeDayList.head)

       s"${formatYear(newMaybeYearList.head.toString)}-${potentialDate.potentialMonth.head}-${newMaybeDayList.head}"

  }

  def simpleValidDayOfMonth(dayOfMonth: Int) : Boolean = dayOfMonth <= 31 && dayOfMonth > 0

//  def complexValidDayOfMonth(dayOfMonth: Int, leapYear: Boolean, month: Int): Boolean = {
//    month match {
//      case 1||3||5||7|8||10|12 => dayOfMonth <= 31
//      case 4||6||9||11 => dayOfMonth <= 31
//      case 2 => if(leapYear) dayOfMonth <= 29 else dayOfMonth <=28
//
//    }
//  }

  def validMonth(month: Int): Boolean = month <= 12 && month > 0

  def validYear(year: Int): Boolean = year >= 0

  def formatYear(year: String): String = if (year.length == 2) {s"20$year"} else year

}
