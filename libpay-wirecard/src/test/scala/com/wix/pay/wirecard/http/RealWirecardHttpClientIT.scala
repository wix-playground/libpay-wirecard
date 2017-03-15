package com.wix.pay.wirecard.http

import com.wix.pay.model.CurrencyAmount
import com.wix.pay.wirecard.{WirecardAuthorization, WirecardMerchant}
import com.wix.pay.{PaymentErrorException, PaymentRejectedException}
import org.specs2.mutable.SpecWithJUnit

class RealWirecardHttpClientIT extends SpecWithJUnit with WirecardHttpClientTestSupport {
  val httpClient = new SprayWirecardHttpClient(WirecardSettings(
    WirecardModeSettings(url = "https://c3.wirecard.com/secure/ssl-gateway", username = "liveUsername", password = "livePassword"),
    WirecardModeSettings(url = "https://c3-test.wirecard.com/secure/ssl-gateway", username = "56500", password = "TestXAPTER")
  ))

  override def wirecardTestCredentials: WirecardMerchant = WirecardMerchant("56500", testMode = true)

  "WirecardHttpClient" should {

    "make successfull purchase" in {
      purchase(successfulPayment) must beASuccessfulTry
    }

    "fail to purchase if transaction is rejected" in {
      purchase(rejectedPayment) must beAFailedTry(beAnInstanceOf[PaymentRejectedException])
    }

    "reject different cards in demo mode" in {
      purchase(successfulPayment, creditCard = realCreditCard) must beAFailedTry(beAnInstanceOf[PaymentRejectedException])
    }

    "fail to purchase if transaction is failed" in {
      purchase(failingPayment) must beAFailedTry(beAnInstanceOf[PaymentErrorException])
    }

    "make successfull authorization and capture" in {
      val auth = preauthorize(successfulPayment).get
      capture(auth, successfulPayment) must beASuccessfulTry
    }

    "capture less amount than authorized" in {
      val auth = preauthorize(successfulPayment).get
      val lessAmount = successfulPayment.currencyAmount.amount - 10

      capture(auth, successfulPayment.copy(currencyAmount = CurrencyAmount("EUR", lessAmount))) must beASuccessfulTry
    }

    "fail to capture more amount than authorized" in {
      val auth = preauthorize(successfulPayment).get
      val higherAmount = successfulPayment.currencyAmount.amount + 10
      capture(auth, successfulPayment.copy(currencyAmount = CurrencyAmount("EUR", higherAmount))) must
        beAFailedTry(beAnInstanceOf[PaymentErrorException])
    }

    "fail to authorize if transaction is failed" in {
      preauthorize(failingPayment) must beAFailedTry(beAnInstanceOf[PaymentErrorException])
    }

    "fail to capture wrong transaction" in {
      capture(WirecardAuthorization("wrong", "wrong"), failingPayment) must
        beAFailedTry(beAnInstanceOf[PaymentErrorException])
    }

    "fail to capture the same auth twice" in {
      val auth = preauthorize(successfulPayment).get

      capture(auth, successfulPayment)
      capture(auth, successfulPayment) must beAFailedTry(beAnInstanceOf[PaymentErrorException])
    }

    "void authorization if it's not captured" in {
      val auth = preauthorize(successfulPayment).get
      voidPreauthorization(auth) must beASuccessfulTry
    }

    "fail to void wrong authorization" in {
      voidPreauthorization(WirecardAuthorization("wrong", "wrong")) must
        beAFailedTry(beAnInstanceOf[PaymentErrorException])
    }

    "fail to void captured authorization" in {
      val auth = preauthorize(successfulPayment).get
      capture(auth, successfulPayment)

      voidPreauthorization(auth) must beAFailedTry(beAnInstanceOf[PaymentErrorException])
    }
  }
}
