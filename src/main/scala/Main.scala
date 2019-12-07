import java.sql.Timestamp

import banking.{BankOperation, BankingFunctions}

object Main extends App {

  val man = "man"
  val woman = "woman"
  val userIdToGender: Map[Int, String] = Map(1 -> man, 2 -> woman)

  val operations: Set[String] = Set("food purchase", "cinema")

  val bankOperationList : Seq[BankOperation] = List(
    banking.BankOperation(Timestamp.valueOf("2019-05-05 13:33:50.234"), 2, 500, "input", 2),
    banking.BankOperation(Timestamp.valueOf("2019-05-05 14:33:50.234"), 1, 500, "input", 1),
    banking.BankOperation(Timestamp.valueOf("2019-05-05 17:00:00.555"), 2, 300, "transfer", 1),
    banking.BankOperation(Timestamp.valueOf("2019-05-05 17:00:00.555"), 1, -300, "transfer", 2),
    banking.BankOperation(Timestamp.valueOf("2019-05-05 17:00:00.555"), 2, 500, "transfer", 1),
    banking.BankOperation(Timestamp.valueOf("2019-05-05 17:00:00.555"), 1, -500, "transfer", 2),
    banking.BankOperation(Timestamp.valueOf("2019-05-05 18:00:00.555"), 1, -1000, "food purchase", 3),
    banking.BankOperation(Timestamp.valueOf("2019-05-05 18:00:00.555"), 2, -400, "food purchase", 3),
    banking.BankOperation(Timestamp.valueOf("2019-05-05 18:00:00.555"), 1, -800, "food purchase", 3),
    banking.BankOperation(Timestamp.valueOf("2019-05-05 18:00:00.555"), 1, -400, "cinema", 3),
    banking.BankOperation(Timestamp.valueOf("2019-05-05 18:00:00.555"), 2, -210, "cinema", 3),
    banking.BankOperation(Timestamp.valueOf("2019-05-05 00:00:00.555"), 2, -220, "cinema", 3),
    banking.BankOperation(Timestamp.valueOf("2019-05-05 05:00:00.555"), 2, -250, "cinema", 3),
    banking.BankOperation(Timestamp.valueOf("2019-05-05 19:20:00.555"), 1, -230, "cinema", 3),
    banking.BankOperation(Timestamp.valueOf("2019-05-05 18:00:00.555"), 1, -250, "cinema", 3),
    banking.BankOperation(Timestamp.valueOf("2019-05-05 19:00:00.555"), 2, 200, "transfer", 1),
    banking.BankOperation(Timestamp.valueOf("2019-05-05 19:00:00.555"), 1, -200, "transfer", 1),
  )

  //2.1
  println(s"Итого по UserID ${BankingFunctions.getTotalByUserId(bankOperationList)}")
  //2.2
  println(s"Наиболее частые операции ${BankingFunctions.getFrequentOperations(bankOperationList)}")
  //2.3
  println(s"Промежуток времени с самым большим количество операций cinema " +
    s"${BankingFunctions.getOperationFilledPeriod(bankOperationList, "cinema")}")
  //3
  println(s"Затраты мужчин и женщин ${BankingFunctions.getExpenseByGender(bankOperationList, userIdToGender)}")
  //4
  println(s"Затраты пользователей по заданным категория ${BankingFunctions.operationsFilter(bankOperationList, operations)}")


}








