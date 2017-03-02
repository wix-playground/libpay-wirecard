package com.wix.pay.wirecard.http

import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.Payment
import com.wix.pay.wirecard.{WirecardAddress, WirecardAuthorization, WirecardMerchant}

import scala.util.Try

trait WirecardHttpClient {

  def purchase(credentials: WirecardMerchant, transactionId: String, creditCard: CreditCard,
               payment: Payment, address: WirecardAddress): Try[String]

  def preauthorize(credentials: WirecardMerchant, transactionId: String, creditCard: CreditCard,
                   payment: Payment, address: WirecardAddress): Try[WirecardAuthorization]

  def capture(credentials: WirecardMerchant, authorization: WirecardAuthorization, amount: Double): Try[String]

  def voidPreauthorization(credentials: WirecardMerchant, authorization: WirecardAuthorization): Try[String]
}
