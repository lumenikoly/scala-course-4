import java.sql.Timestamp
import java.text.SimpleDateFormat

object Main extends App {

  val man = "man"
  val woman = "woman"
  val userIdToGender: Map[Int, String] = Map(1 -> man, 2 -> woman)

  val operations: Set[String] = Set("food purchase", "cinema")

  val bankOperationList : Seq[BankOperation] = List(
    BankOperation(Timestamp.valueOf("2019-05-05 13:33:50.234"), 0, 500, "input", 0),
    BankOperation(Timestamp.valueOf("2019-05-05 14:33:50.234"), 1, 1500, "input", 0),
    BankOperation(Timestamp.valueOf("2019-05-05 17:00:00.555"), 0, 300, "transfer", 1),
    BankOperation(Timestamp.valueOf("2019-05-05 17:00:00.555"), 1, -300, "transfer", 0),
    BankOperation(Timestamp.valueOf("2019-05-05 17:00:00.555"), 2, 500, "transfer", 1),
    BankOperation(Timestamp.valueOf("2019-05-05 17:00:00.555"), 1, -500, "transfer", 2),
    BankOperation(Timestamp.valueOf("2019-05-05 18:00:00.555"), 1, -1000, "food purchase", 3),
    BankOperation(Timestamp.valueOf("2019-05-05 18:00:00.555"), 0, -400, "food purchase", 3),
    BankOperation(Timestamp.valueOf("2019-05-05 18:00:00.555"), 0, -800, "food purchase", 3),
    BankOperation(Timestamp.valueOf("2019-05-05 18:00:00.555"), 0, -800, "cinema", 3),
    BankOperation(Timestamp.valueOf("2019-05-05 18:00:00.555"), 2, -200, "cinema", 3),
    BankOperation(Timestamp.valueOf("2019-05-05 00:00:00.555"), 2, -200, "cinema", 3),
    BankOperation(Timestamp.valueOf("2019-05-05 05:00:00.555"), 2, -250, "cinema", 3),
    BankOperation(Timestamp.valueOf("2019-05-05 19:20:00.555"), 1, -200, "cinema", 3),
    BankOperation(Timestamp.valueOf("2019-05-05 08:00:00.555"), 1, -250, "cinema", 3),
    BankOperation(Timestamp.valueOf("2019-05-05 19:00:00.555"), 0, 200, "transfer", 1),
    BankOperation(Timestamp.valueOf("2019-05-05 19:00:00.555"), 1, -200, "transfer", 0),
  )

  //2.1
  def totalByUserId(list: Seq[BankOperation]): Map[Int, Double] = {
     list.groupBy(_.userId).map {
      case (userId, total) =>
        val totals = total.map(_.operation).distinct.sum
        (userId, totals)
    }
  }

    //2.2
    def frequentOperations(list: Seq[BankOperation]): Map[String, Int] = {
      def deleteDubTransfer(list: Seq[BankOperation])= {
        val transferList = list.sliding(2).collect {
          case Seq(a : BankOperation, b: BankOperation) if  a.eventDate == b.eventDate &&
            a.source == b.userId &&
            a.operation == -b.operation => a
        }
        transferList
      }

      list.sortBy(_.eventDate).
        groupBy(_.title).
        map {
          case (title, operation) if title == "transfer" =>
            val transferList = deleteDubTransfer(operation)
            (title, transferList.size)
          case (title, operation) =>
            (title, operation.size)
        }
    }




  //2.3
  def timePeriodWithLargestNumberOfOperations(list: Seq[BankOperation], titleOperation: String) = {
    def timeFilter(evenDate: Timestamp, initialHour: Int, finalHour: Int): Boolean = {
      val dateFormat = new SimpleDateFormat("HH")
      val time = dateFormat.format(evenDate)
      if(time.toInt >= initialHour && time.toInt < finalHour) {
        true
      } else false
    }
    val operationSortedByTime = list.view.filter(_.title == titleOperation).
      map {
        case list if timeFilter(list.eventDate, 0, 6) =>
          ("C 00 до 06", list)
        case list if timeFilter(list.eventDate, 6, 12) =>
          ("C 06 до 12", list)
        case list if timeFilter(list.eventDate, 12, 18) =>
          ("C 12 до 18", list)
        case list if timeFilter(list.eventDate, 18, 24) =>
          ("C 18 до 24", list)
      }
    val a: Map[String, Int] = operationSortedByTime.view.groupMap(_._1)(_._2).
      map {
        case (time, operation) =>
         val a = operation.headOption
          (time, a.size)
      }
    val maxValue = a.values.maxByOption(x => x) match {
      case Some(value) => value
      case None => None
    }
    a.filter(_._2 == maxValue).keys
  }

  //3
  def genderExpense(list: Seq[BankOperation], gender: Map[Int, String]) = {
    val genderListExpense: Seq[BankOperation] = list.filter(x => gender.contains(x.userId) && x.operation < 0)
    val sumExpense: Map[Int, Double] = totalByUserId(genderListExpense)
    sumExpense
  }

  //4
  def operationsFilter(list:  Seq[BankOperation], operationsTitle: Set[String]): Map[Int, Map[String, Double]] = {
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


  println(s"2.1 Итого по UserID ${totalByUserId(bankOperationList)}")
  println(s"2.2 Наиболее частые операции ${frequentOperations(bankOperationList)}")
  println(s"2.3 Промежуток времени с самым большим количество операций cinema " +
    s"${timePeriodWithLargestNumberOfOperations(bankOperationList, "cinema")}")
  println(s"4 Затраты пользователя по заданным категория ${operationsFilter(bankOperationList, operations)}")
  println(s"3 Затраты мужчини и женщин ${genderExpense(bankOperationList, userIdToGender)}")


}








