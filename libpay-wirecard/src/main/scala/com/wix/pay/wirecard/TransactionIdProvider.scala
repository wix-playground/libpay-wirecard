package com.wix.pay.wirecard

import java.util.UUID

trait TransactionIdProvider {
  def nextTransactionId: String
}

object RandomTransactionIdProvider extends TransactionIdProvider {
  override def nextTransactionId = UUID.randomUUID.toString.replace("-", "").take(32)
}