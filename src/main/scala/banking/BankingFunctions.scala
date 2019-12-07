package banking

import java.sql.Timestamp
import java.text.SimpleDateFormat

import scala.collection.View

object BankingFunctions {
  //2.1
  def getTotalByUserId(list: Seq[BankOperation]): Map[Int, Double] = {
    list.groupBy(_.userId).map {
      case (userId, total) =>
        val totals = total.map(_.operation).distinct.sum
        (userId, totals)
    }
  }

  //2.2
  def getFrequentOperations(list: Seq[BankOperation]): Map[String, Int] = {
    def deleteDubTransfer(list: Seq[BankOperation])= {
      val transferList = list.sliding(2).collect {
        case Seq(a : BankOperation, b: BankOperation) if  a.eventDate == b.eventDate &&
          a.source == b.userId &&
          a.operation == -b.operation => a
      }
      transferList
    }

   val listGroupedByTitle = list.sortBy(_.eventDate).
      groupBy(_.title).
      map {
        case (title, operation) if title == "transfer" =>
          val transferList = deleteDubTransfer(operation)
          (title, transferList.size)
        case (title, operation) =>
          (title, operation.size)
      }
    val maxValue = listGroupedByTitle.values.maxByOption(x => x) match {
      case Some(value) => value
      case None => None
    }
    listGroupedByTitle.filter(_._2 == maxValue)
  }


  //2.3
  def getOperationFilledPeriod(list: Seq[BankOperation], titleOperation: String): Iterable[String] = {
    def timeFilter(evenDate: Timestamp, initialHour: Int, finalHour: Int): Boolean = {
      val dateFormat = new SimpleDateFormat("HH")
      val time = dateFormat.format(evenDate)
      if(time.toInt >= initialHour && time.toInt < finalHour) {
        true
      } else false
    }
    val operationSortedByTime: View[(String, BankOperation)] = list.view.filter(_.title == titleOperation).
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
    val groupOperationsByTime: Map[String, Int] = operationSortedByTime.view.groupMap(_._1)(_._2).
      map {
        case (time, operation) =>
          val operationOption = operation.headOption
          (time, operationOption.size)
      }
    val maxValue = groupOperationsByTime.values.maxByOption(x => x) match {
      case Some(value) => value
      case None => None
    }
    groupOperationsByTime.filter(_._2 == maxValue).keys
  }

  //3
  def getExpenseByGender(list: Seq[BankOperation], gender: Map[Int, String]): Map[Int, Double] = {
    val genderListExpense: Seq[BankOperation] = list.filter(x => gender.contains(x.userId) && x.operation < 0)
    val sumExpense: Map[Int, Double] = getTotalByUserId(genderListExpense)
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

}
