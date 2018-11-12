package com.wix.pay.wirecard.http


import com.wix.pay.model.Payment
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.{BeforeEach, Scope}
import com.wix.pay.wirecard.{WirecardAuthorization, WirecardMerchant}
import com.wix.pay.wirecard.testkit.{WirecardAppCredentials, WirecardDriver}
import com.wix.pay.{PaymentErrorException, PaymentRejectedException}
import org.specs2.matcher.Matcher

import scala.util.Try


class SandboxWirecardHttpClientIT extends SpecWithJUnit with WirecardHttpClientTestSupport with BeforeEach{
  val driver = new WirecardDriver(port = 10001)
  override val wirecardTestCredentials = WirecardMerchant("56501", testMode = true)

  val liveAppCredentials = WirecardAppCredentials(username = "liveUsername", password = "livePassword")
  val testAppCredentials = WirecardAppCredentials(username = "testUsername", password = "testPassword")

  val appCredentials: WirecardAppCredentials = testAppCredentials

  override protected def before: Any = driver.reset()


  sequential


  step {
    driver.start()
  }


  "authorize request" should {
    "successfully yield an authorization key upon a valid request" in new Ctx {
      givenWirecardAuthorizationRequest getsValidWith resultGuWid
      preauthorize(somePayment) must beSuccessfulTry(WirecardAuthorization(resultGuWid, transactionId))
    }

    "fail with PaymentRejectedException for rejected transactions" in new Ctx {
      givenWirecardAuthorizationRequest getsRejectedWith("error", "advice")
      preauthorize(somePayment) must beFailedTry(be_==(PaymentRejectedException("error Advice: advice")))
    }

    "fail with PaymentRejectedException with guWid for rejected transactions only guWid exists in response" in new Ctx {
      givenWirecardAuthorizationRequest getsRejectedWith("error", "advice", guWid = someOptionGuWid)
      preauthorize(somePayment) must beFailedTry(be_==(PaymentRejectedException("error Advice: advice", transactionId = someOptionGuWid)))
    }

    "fail with PaymentRejectedException with transactionId for rejected transactions when only transactionId exists in response" in new Ctx {
      givenWirecardAuthorizationRequest getsRejectedWith("error", "advice", transactionId = someOptionTransactionId)
      preauthorize(somePayment) must beFailedTry(be_==(PaymentRejectedException("error Advice: advice",  someOptionTransactionId)))
    }

    "fail with PaymentRejectedException with guWid for rejected transactions when guWid and transactionId exists in response" in new Ctx {
      givenWirecardAuthorizationRequest getsRejectedWith("error", "advice", guWid = someOptionGuWid, transactionId = someOptionTransactionId)
      preauthorize(somePayment) must beFailedTry(be_==(PaymentRejectedException("error Advice: advice", someOptionGuWid)))
    }

    "fail with PaymentErrorException for erroneous transactions" in new Ctx {
      givenWirecardAuthorizationRequest getsFailedWith ("error", "advice")
      preauthorize(somePayment) must beFailedTry(be_==(PaymentErrorException("error Advice: advice")))
    }

    "fail with bad status code" in new Ctx {
      givenWirecardAuthorizationRequest getsFailedOnServer()
      preauthorize(somePayment) must beFailedTry(be_==(PaymentErrorException("HTTP error. Status: 500")))
    }

    "fail with wrong businessCaseSignature" in new Ctx {
      givenWirecardAuthorizationRequest getsFailedWithWrongBusinessCaseSignature()
      preauthorize(somePayment) must beFailedTry(be_==(PaymentErrorException(s"Wrong BC Advice: Business Case Signature " +
        s"'${wirecardTestCredentials.businessCaseSignature}' is not accepted")))
    }
  }


  "capture request" should {
    "successfully capture valid transaction" in new Ctx {
      givenWirecardCaptureRequest getsValidWith resultGuWid
      capture(someAuth, somePayment) must beSuccessfulTry(resultGuWid)
    }

    "fail with PaymentRejectedException for rejected capture" in new Ctx {
      givenWirecardCaptureRequest getsRejectedWith("error", "advice")
      capture(someAuth, somePayment) must beFailedTry(be_==(PaymentRejectedException("error Advice: advice")))
    }

    "fail with PaymentRejectedException with GuWid for rejected capture when only guWid exists" in new Ctx {
      givenWirecardCaptureRequest getsRejectedWith("error", "advice", guWid = someOptionGuWid)
      capture(someAuth, somePayment) must beFailedTry(be_==(PaymentRejectedException("error Advice: advice", someOptionGuWid)))
    }

    "fail with PaymentRejectedException with transactionId for rejected capture when only transactionId exists" in new Ctx {
      givenWirecardCaptureRequest getsRejectedWith("error", "advice", transactionId = someOptionTransactionId)
      capture(someAuth, somePayment) must beFailedTry(be_==(PaymentRejectedException("error Advice: advice", someOptionTransactionId)))
    }

    "fail with PaymentRejectedException with GuWid for rejected capture when guWid and transactionId exists" in new Ctx {
      givenWirecardCaptureRequest getsRejectedWith("error", "advice", guWid = someOptionGuWid, transactionId = someOptionTransactionId)
      capture(someAuth, somePayment) must beFailedTry(be_==(PaymentRejectedException("error Advice: advice", someOptionGuWid)))
    }
  }


  "purchase request" should {
    "successfully do valid purchase" in new Ctx {
      givenWirecardPurchaseRequest getsValidWith resultGuWid
      purchase(somePayment) must beSuccessfulTry(resultGuWid)
    }

    "fail with PaymentRejectedException for rejected purchase" in new Ctx {
      givenWirecardPurchaseRequest getsRejectedWith("error", "advice")
      purchase(somePayment) must beFailedTry(be_==(PaymentRejectedException("error Advice: advice")))
    }

    "fail with PaymentRejectedException with GuWid for rejected capture when only guWid exists" in new Ctx {
      givenWirecardPurchaseRequest getsRejectedWith("error", "advice", guWid = someOptionGuWid)
      purchase(somePayment) must beFailedTry(be_==(PaymentRejectedException("error Advice: advice", someOptionGuWid)))
    }

    "fail with PaymentRejectedException with transactionId for rejected capture when only transactionId exists" in new Ctx {
      givenWirecardPurchaseRequest getsRejectedWith("error", "advice", transactionId = someOptionTransactionId)
      purchase(somePayment) must beFailedTry(be_==(PaymentRejectedException("error Advice: advice", someOptionTransactionId)))
    }

    "fail with PaymentRejectedException with GuWid for rejected capture when guWid and transactionId exists" in new Ctx {
      givenWirecardPurchaseRequest getsRejectedWith("error", "advice", guWid = someOptionGuWid, transactionId = someOptionTransactionId)
      purchase(somePayment) must beFailedTry(be_==(PaymentRejectedException("error Advice: advice", someOptionGuWid)))
    }
  }


  "void auth request" should {
    "successfully void auth for valid auth" in new Ctx {
      givenWirecardVoidAuthRequest getsValidWith resultGuWid
      voidPreauthorization(someAuth) must beSuccessfulTry(resultGuWid)
    }

    "fail with PaymentErrorException for rejected purchase" in new Ctx {
      givenWirecardVoidAuthRequest getsFailedWith ("error", "advice")
      voidPreauthorization(someAuth) must beFailedTry(be_==(PaymentErrorException("error Advice: advice")))
    }

    "fail with PaymentErrorException with GuWid for rejected capture when only guWid exists" in new Ctx {
      givenWirecardVoidAuthRequest getsFailedWith("error", "advice", guWid = someOptionGuWid)
      voidPreauthorization(someAuth) must beFailedTry(be_==(PaymentErrorException("error Advice: advice", someOptionGuWid)))
    }

    "fail with PaymentErrorException with transactionId for rejected capture when only transactionId exists" in new Ctx {
      givenWirecardVoidAuthRequest getsFailedWith("error", "advice", transactionId = someOptionTransactionId)
      voidPreauthorization(someAuth) must beFailedTry(be_==(PaymentErrorException("error Advice: advice", someOptionTransactionId)))
    }

    "fail with PaymentErrorException with GuWid for rejected capture when guWid and transactionId exists" in new Ctx {
      givenWirecardVoidAuthRequest getsFailedWith("error", "advice", guWid = someOptionGuWid, transactionId = someOptionTransactionId)
      voidPreauthorization(someAuth) must beFailedTry(be_==(PaymentErrorException("error Advice: advice", someOptionGuWid)))
    }
  }


  "Wirecard http client" should {
    "fail on wrong credentials" in new Ctx {
      givenWirecardAuthorizationRequest getsValidWith resultGuWid
      preauthorize(somePayment, wrongMerchantCredentials) must beFailedTry.withThrowable[PaymentErrorException]
    }
  }


  step {
    driver.stop()
  }


  trait Ctx extends Scope {
    val liveMerchantCredentials: WirecardMerchant = wirecardTestCredentials.copy(testMode = false)
    val wrongMerchantCredentials: WirecardMerchant = wirecardTestCredentials.copy(businessCaseSignature = "wrong")

    val someGuWid = "someGuWid"
    val someAuth = WirecardAuthorization(someGuWid, transactionId)
    val resultGuWid = "resultGuWid"
    val someOptionTransactionId = Some(transactionId)
    val someOptionGuWid = Some(resultGuWid)

    val somePayment: Payment = successfulPayment

    def givenWirecardAuthorizationRequest: driver.PreAuthorizationRequest = driver.aPreauthorizationRequest(wirecardTestCredentials, appCredentials,
      transactionId, testCreditCard, somePayment, testWirecardAddress)

    def givenWirecardCaptureRequest: driver.CaptureRequest = driver.aCaptureRequest(wirecardTestCredentials, appCredentials, someAuth,
      somePayment.currencyAmount.amount)

    def givenWirecardPurchaseRequest: driver.PurchaseRequest = driver.aPurchaseRequest(wirecardTestCredentials, appCredentials,
      transactionId, testCreditCard, somePayment, testWirecardAddress)

    def givenWirecardVoidAuthRequest: driver.VoidAuthorizationRequest = driver.aVoidAuthorizationRequest(wirecardTestCredentials, appCredentials, someAuth)

    def hasMessage(message: String): AnyRef with Matcher[Throwable] =
      be_==("Connection attempt to localhost:10002 failed") ^^ ((e: Throwable) => e.getMessage)
  }

  val httpClient = new AkkaWirecardHttpClient(WirecardSettings(
    liveSettings = WirecardModeSettings(url = "http://localhost:10002", username = liveAppCredentials.username, password = liveAppCredentials.password),
    testSettings = WirecardModeSettings(url = "http://localhost:10001", username = testAppCredentials.username, password = testAppCredentials.password)))

}

class LiveWirecardHttpClientIT extends SandboxWirecardHttpClientIT {
  override val driver = new WirecardDriver(port = 10002)
  override val wirecardTestCredentials = WirecardMerchant("56501")

  override val appCredentials: WirecardAppCredentials = liveAppCredentials
}
