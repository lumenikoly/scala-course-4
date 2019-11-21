import java.sql.Timestamp
import java.text.SimpleDateFormat

object Main extends App {
  val SystemId = 0

  val man = "man"
  val woman = "woman"
  val userIdToGender: Map[Int, String] = Map(1 -> man, 2 -> woman)

  val operations: Set[String] = Set("food purchase", "cinema")

  val bankOperationList : List[BankOperation] = List(
    BankOperation(Timestamp.valueOf("2019-05-05 13:33:50.234"), 0, 500, "input", SystemId),
    BankOperation(Timestamp.valueOf("2019-05-05 14:33:50.234"), 1, 1500, "input", SystemId),
    BankOperation(Timestamp.valueOf("2019-05-05 17:00:00.555"), 0, 300, "transfer", 1),
    BankOperation(Timestamp.valueOf("2019-05-05 17:00:00.555"), 1, -500, "transfer", 0),
    BankOperation(Timestamp.valueOf("2019-05-05 18:00:00.555"), 1, -1000, "food purchase", 3),
    BankOperation(Timestamp.valueOf("2019-05-05 18:00:00.555"), 0, -400, "food purchase", 3),
    BankOperation(Timestamp.valueOf("2019-05-05 18:00:00.555"), 0, -800, "food purchase", 3),
    BankOperation(Timestamp.valueOf("2019-05-05 18:00:00.555"), 0, -800, "cinema", 3),
    BankOperation(Timestamp.valueOf("2019-05-05 18:00:00.555"), 2, -200, "cinema", 3),
    BankOperation(Timestamp.valueOf("2019-05-05 00:00:00.555"), 2, -200, "cinema", 3),
    BankOperation(Timestamp.valueOf("2019-05-05 05:00:00.555"), 2, -250, "cinema", 3),
    BankOperation(Timestamp.valueOf("2019-05-05 19:00:00.555"), 0, 200, "transfer", 1),
    BankOperation(Timestamp.valueOf("2019-05-05 19:00:00.555"), 1, -200, "transfer", 0),
  )

  //2.1
  def totalByUserId(list: List[BankOperation]): Map[Int, Double] = {
      list.groupBy(_.userId).
      map {
      case (userId, total) =>
        val totals = total.map(_.operation).distinct.sum
        (userId, totals)
    }
  }

  //2.2
  def frequentOperations(list: List[BankOperation]): Map[String, Int] = {
    list.sortBy(_.eventDate).
      groupBy(_.title).
      map {
      case (title, operation) if title == "transfer" =>
        val transferList = (operation, operation.tail).zipped.drop(1)
         (title, transferList.size)
      case (title, operation) =>
         (title, operation.size)
    }
  }

  //2.3
  def data(list: List[BankOperation], titleOperation: String) = {
    def timeFilter(evenDate: Timestamp, initialHour: Int, finalHour: Int): Boolean = {
      val dateFormat = new SimpleDateFormat("HH")
      val time = dateFormat.format(evenDate)
      if(time.toInt >= initialHour && time.toInt < finalHour) {
        true
      } else false
    }
    val operationSortedByTime: Map[String, Double] = list.view.filter(_.title == titleOperation).
      map {
        case list if timeFilter(list.eventDate, 0, 6) =>
          ("C 00 до 06", list.operation)
        case list if timeFilter(list.eventDate, 6, 12) =>
          ("C 06 до 12", list.operation)
        case list if timeFilter(list.eventDate, 12, 18) =>
          ("C 12 до 18", list.operation)
        case list if timeFilter(list.eventDate, 18, 24) =>
          ("C 18 до 24", list.operation)
      }.groupMap(_._1)(_._2).
      map {
        case (time, operation) => (time, operation.sum)
      }

      operationSortedByTime.
        toSeq.
        minByOption(_._2) match {
          case Some(value) => value._1
          case None => "Операций не найдено"
        }
  }

  //3
  def genderExpense(list: List[BankOperation], gender: Map[Int, String]) = {
    val genderListExpense: List[BankOperation] = list.filter(x => gender.contains(x.userId) && x.operation < 0)
    val sumExpense: Map[Int, Double] = totalByUserId(genderListExpense)
    sumExpense
  }

  //4
  def operationsFilter(list: List[BankOperation], operationsTitle: Set[String]): Map[Int, Map[String, Double]] = {
      list.
      filter(x => operationsTitle.contains(x.title)).
      groupBy(_.userId).
      map {
        case (userId, operations) =>
          val filteredOperation: Map[String, Double] = operations.groupBy(_.title).map {
            case (title, operation) =>
            val sumOperation: Double = operation.map(_.operation).sum
              (title, sumOperation)
          }
          (userId, filteredOperation)
      }

  }


  println(totalByUserId(bankOperationList))
  println(frequentOperations(bankOperationList))
  println(operationsFilter(bankOperationList, operations))
  println(genderExpense(bankOperationList, userIdToGender))
  println(data(bankOperationList, "cinema"))

 // println(A.l)

}








