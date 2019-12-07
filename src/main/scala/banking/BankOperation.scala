package banking

import java.sql.Timestamp

case class  BankOperation(eventDate: Timestamp, userId: Int, operation: Double, title: String, source: Int)
