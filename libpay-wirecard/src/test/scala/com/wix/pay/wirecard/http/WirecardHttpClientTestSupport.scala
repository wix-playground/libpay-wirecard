package com.wix.pay.wirecard.http

import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.model.{CurrencyAmount, Payment}
import com.wix.pay.wirecard.{RandomTransactionIdProvider, WirecardAddress, WirecardAuthorization, WirecardMerchant}

trait WirecardHttpClientTestSupport {

  def httpClient: WirecardHttpClient

  val transactionId = RandomTransactionIdProvider.nextTransactionId

  def wirecardTestCredentials: WirecardMerchant

  val testCreditCard = CreditCard("4200000000000000", YearMonth(2019, 1),
    Some(CreditCardOptionalFields.withFields(csc = Some("471"), holderName = Some("John Doe"))))

  val realCreditCard = testCreditCard.copy(number = "4222222222222")

  val testWirecardAddress = WirecardAddress(firstName = "John", lastName = "Doe", address1 = "52 st. 278",
    city = "New York", zipCode = "10010", state = "NY", country = "US", email = "john@example.com")

  val successfulPayment = Payment(CurrencyAmount("EUR", 20.25))

  val rejectedPayment = Payment(CurrencyAmount("EUR", 1000.02))

  val failingPayment = Payment(CurrencyAmount("EUR", 1002.25))

  def purchase(payment: Payment, credentials: WirecardMerchant = wirecardTestCredentials,
               creditCard: CreditCard = testCreditCard, address: WirecardAddress = testWirecardAddress) =
    httpClient.purchase(credentials, transactionId, creditCard, payment, address)

  def preauthorize(payment: Payment, credentials: WirecardMerchant = wirecardTestCredentials,
               creditCard: CreditCard = testCreditCard, address: WirecardAddress = testWirecardAddress) =
    httpClient.preauthorize(credentials, transactionId, creditCard, payment, address)

  def capture(auth: WirecardAuthorization, payment: Payment, credentials: WirecardMerchant = wirecardTestCredentials) =
    httpClient.capture(credentials, auth, payment.currencyAmount.amount)

  def voidPreauthorization(auth: WirecardAuthorization, credentials: WirecardMerchant = wirecardTestCredentials) =
    httpClient.voidPreauthorization(credentials, auth)
}
