package com.wix.pay.wirecard.http

import com.wix.pay.model.CurrencyAmount
import com.wix.pay.wirecard.WirecardAuthorization
import com.wix.pay.{PaymentErrorException, PaymentRejectedException}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

class RealWirecardHttpClientIT extends SpecWithJUnit {

  "WirecardHttpClient" should {

    "make successfull purchase" in new Ctx {
      purchase(successfulPayment) must beASuccessfulTry
    }

    "fail to purchase if transaction is rejected" in new Ctx {
      purchase(rejectedPayment) must beAFailedTry(beAnInstanceOf[PaymentRejectedException])
    }

    "reject different cards in demo mode" in new Ctx {
      purchase(successfulPayment, creditCard = realCreditCard) must beAFailedTry(beAnInstanceOf[PaymentRejectedException])
    }

    "fail to purchase if transaction is failed" in new Ctx {
      purchase(failingPayment) must beAFailedTry(beAnInstanceOf[PaymentErrorException])
    }

    "make successfull authorization and capture" in new Ctx {
      val auth = preauthorize(successfulPayment).get
      capture(auth, successfulPayment) must beASuccessfulTry
    }

    "capture less amount than authorized" in new Ctx {
      val auth = preauthorize(successfulPayment).get
      val lessAmount = successfulPayment.currencyAmount.amount - 10

      capture(auth, successfulPayment.copy(currencyAmount = CurrencyAmount("EUR", lessAmount))) must beASuccessfulTry
    }

    "fail to capture more amount than authorized" in new Ctx {
      val auth = preauthorize(successfulPayment).get
      val higherAmount = successfulPayment.currencyAmount.amount + 10
      capture(auth, successfulPayment.copy(currencyAmount = CurrencyAmount("EUR", higherAmount))) must
        beAFailedTry(beAnInstanceOf[PaymentErrorException])
    }

    "fail to authorize if transaction is failed" in new Ctx {
      preauthorize(failingPayment) must beAFailedTry(beAnInstanceOf[PaymentErrorException])
    }

    "fail to capture wrong transaction" in new Ctx {
      capture(WirecardAuthorization("wrong", "wrong"), failingPayment) must
        beAFailedTry(beAnInstanceOf[PaymentErrorException])
    }

    "fail to capture the same auth twice" in new Ctx {
      val auth = preauthorize(successfulPayment).get

      capture(auth, successfulPayment)
      capture(auth, successfulPayment) must beAFailedTry(beAnInstanceOf[PaymentErrorException])
    }

    "void authorization if it's not captured" in new Ctx {
      val auth = preauthorize(successfulPayment).get
      voidPreauthorization(auth) must beASuccessfulTry
    }

    "fail to void wrong authorization" in new Ctx {
      voidPreauthorization(WirecardAuthorization("wrong", "wrong")) must
        beAFailedTry(beAnInstanceOf[PaymentErrorException])
    }

    "fail to void captured authorization" in new Ctx {
      val auth = preauthorize(successfulPayment).get
      capture(auth, successfulPayment)

      voidPreauthorization(auth) must beAFailedTry(beAnInstanceOf[PaymentErrorException])
    }
  }

  trait Ctx extends Scope with WirecardHttpClientTestSupport {
    val httpClient = new SprayWirecardHttpClient(WirecardUrls(
      liveUrl = "https://c3.wirecard.com/secure/ssl-gateway",
      testUrl = "https://c3-test.wirecard.com/secure/ssl-gateway"
    ))
  }
}
