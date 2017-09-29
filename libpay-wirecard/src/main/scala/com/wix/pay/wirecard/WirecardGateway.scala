package com.wix.pay.wirecard

import com.wix.pay.PaymentGateway
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.{Customer, Deal, Payment}
import com.wix.pay.wirecard.http.{SprayWirecardHttpClient, WirecardHttpClient, WirecardSettings}
import com.wix.pay.wirecard.parsers.{JsonWirecardAuthorizationParser, JsonWirecardMerchantParser}

import scala.util.Try

class WirecardGateway(httpClient: WirecardHttpClient,
                      transactionIdProvider: TransactionIdProvider = RandomTransactionIdProvider)
  extends PaymentGateway {

  def this(wirecardSettings: WirecardSettings) = this(new SprayWirecardHttpClient(wirecardSettings))

  private val merchantParser = new JsonWirecardMerchantParser
  private val authorizationParser = new JsonWirecardAuthorizationParser

  override def sale(merchantKey: String, creditCard: CreditCard, payment: Payment,
                    customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    require(payment.installments == 1, "Wirecard doesn't support installments")

    val creds = merchantParser.parse(merchantKey)
    for {
      auth <- httpClient.preauthorize(
        creds,
        transactionIdProvider.nextTransactionId,
        creditCard,
        payment,
        WirecardAddress.Empty.withCustomer(customer).withDetailedAddress(creditCard.billingAddressDetailed))
      capture <- httpClient.capture(creds, auth, payment.currencyAmount.amount)
    } yield capture
  }

  override def authorize(merchantKey: String, creditCard: CreditCard, payment: Payment,
                         customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    require(payment.installments == 1, "Wirecard doesn't support installments")

    httpClient.preauthorize(
      merchantParser.parse(merchantKey),
      transactionIdProvider.nextTransactionId,
      creditCard,
      payment,
      WirecardAddress.Empty.withCustomer(customer).withDetailedAddress(creditCard.billingAddressDetailed)
    ).map(authorizationParser.stringify)
  }

  override def capture(merchantKey: String, authorizationKey: String, amount: Double): Try[String] =
    httpClient.capture(
      merchantParser.parse(merchantKey),
      authorizationParser.parse(authorizationKey),
      amount
    )

  override def voidAuthorization(merchantKey: String, authorizationKey: String): Try[String] =
    httpClient.voidPreauthorization(
      merchantParser.parse(merchantKey),
      authorizationParser.parse(authorizationKey)
    )
}
