object DateFormatter {

  def input(inputString: String): String = {

    val inputList: List[Int] = inputString.split('/').toList.map(_.toInt)
    var maybeMonthList: List[Int] = List.empty
    var maybeDayList: List[Int] = List.empty
    val maybeYearList: List[Int] = inputList

    maybeMonthList = inputList.flatMap{ segment =>
      Some(segment).filter(validMonth)
    }

    maybeDayList = inputList.flatMap{ segment =>
      Some(segment).filter(simpleValidDayOfMonth)
    }

    handleMaybeLists(maybeDayList, maybeMonthList, maybeYearList)
  }


  def handleMaybeLists(maybeDayList: List[Int] , maybeMonthList: List[Int], maybeYearList: List[Int]): String = {

    if (maybeMonthList.isEmpty || maybeDayList.isEmpty) return "INVALID DATE"

    var newMaybeDayList = maybeDayList.filter(maybeDay => maybeDay != maybeMonthList.head)
    var newMaybeYearList = maybeYearList.filter(maybeYear => maybeYear != maybeMonthList.head)
    newMaybeYearList = newMaybeYearList.filter(maybeYear => maybeYear != newMaybeDayList.head)
    newMaybeYearList = newMaybeYearList.filter(maybeYear => maybeYear != maybeDayList.head)


    if (newMaybeYearList.isEmpty || newMaybeYearList.isEmpty
    ) {
      "INVALID DATE"
    }
    else {
       s"${formatYear(newMaybeYearList.head.toString)}-${maybeMonthList.head}-${newMaybeDayList.head}"
    }
  }

  def simpleValidDayOfMonth(dayOfMonth: Int) : Boolean = dayOfMonth <= 31

//  def complexValidDayOfMonth(dayOfMonth: Int, leapYear: Boolean, month: Int): Boolean = {
//    month match {
//      case 1||3||5||7|8||10|12 => dayOfMonth <= 31
//      case 4||6||9||11 => dayOfMonth <= 31
//      case 2 => if(leapYear) dayOfMonth <= 29 else dayOfMonth <=28
//
//    }
//  }

  def validMonth(month: Int): Boolean = month <= 12

  def formatYear(year: String): String = if (year.length == 2) {s"20$year"} else year

}
