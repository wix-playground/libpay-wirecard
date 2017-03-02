package com.wix.pay.wirecard.http

import com.wix.pay.wirecard.WirecardAuthorization
import com.wix.pay.wirecard.testkit.WirecardDriver
import com.wix.pay.{PaymentErrorException, PaymentRejectedException}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

class WirecardHttpClientIT extends SpecWithJUnit {
  val probePort = 10001
  val driver = new WirecardDriver(probePort)

  sequential
  step {
    driver.start()
  }

  "authorize request" should {
    "successfully yield an authorization key upon a valid request" in new Ctx {
      givenWirecardAuthorizationRequest isValidWith resultGuWid
      preauthorize(somePayment) must beSuccessfulTry(WirecardAuthorization(resultGuWid, transactionId))
    }

    "fail with PaymentRejectedException for rejected transactions" in new Ctx {
      givenWirecardAuthorizationRequest isRejectedWith("error", "advice")
      preauthorize(somePayment) must beFailedTry(PaymentRejectedException("error Advice: advice"))
    }

    "fail with PaymentErrorException for erroneous transactions" in new Ctx {
      givenWirecardAuthorizationRequest isFailedWith ("error", "advice")
      preauthorize(somePayment) must beFailedTry(PaymentErrorException("error Advice: advice"))
    }

    "fail with bad status code" in new Ctx {
      givenWirecardAuthorizationRequest isFailedOnServer()
      preauthorize(somePayment) must beFailedTry(PaymentErrorException("HTTP error. Status: 500"))
    }
  }

  "capture request" should {
    "successfully capture valid transaction" in new Ctx {
      givenWirecardCaptureRequest isValidWith resultGuWid
      capture(someAuth, somePayment) must beSuccessfulTry(resultGuWid)
    }

    "fail with PaymentRejectedException for rejected capture" in new Ctx {
      givenWirecardCaptureRequest isRejectedWith("error", "advice")
      capture(someAuth, somePayment) must beFailedTry(PaymentRejectedException("error Advice: advice"))
    }
  }

  "purchase request" should {
    "successfully do valid purchase" in new Ctx {
      givenWirecardPurchaseRequest isValidWith resultGuWid
      purchase(somePayment) must beSuccessfulTry(resultGuWid)
    }

    "fail with PaymentRejectedException for rejected purchase" in new Ctx {
      givenWirecardPurchaseRequest isRejectedWith("error", "advice")
      purchase(somePayment) must beFailedTry(PaymentRejectedException("error Advice: advice"))
    }
  }

  "void auth request" should {
    "successfully void auth for valid auth" in new Ctx {
      givenWirecardVoidAuthRequest isValidWith resultGuWid
      voidPreauthorization(someAuth) must beSuccessfulTry(resultGuWid)
    }

    "fail with PaymentRejectedException for rejected purchase" in new Ctx {
      givenWirecardVoidAuthRequest isFailedWith ("error", "advice")
      voidPreauthorization(someAuth) must beFailedTry(PaymentErrorException("error Advice: advice"))
    }
  }

  "Wirecard http client" should {
    "fail on wrong credentials" in new Ctx {
      givenWirecardAuthorizationRequest isValidWith resultGuWid
      preauthorize(somePayment, wrongCredentials) must beFailedTry(PaymentErrorException("HTTP error. Status: 401"))
    }

    "send request to port 10002 on live credentials" in new Ctx {
      preauthorize(somePayment, liveCredentials) must beFailedTry(
        hasMessage("Connection attempt to localhost:10002 failed"))
    }
  }

  step {
    driver.stop()
  }

  trait Ctx extends Scope with WirecardHttpClientTestSupport {
    val httpClient = new SprayWirecardHttpClient(WirecardUrls("http://localhost:10002", "http://localhost:10001"))

    val liveCredentials = wirecardTestCredentials.copy(testMode = false)
    val wrongCredentials = wirecardTestCredentials.copy(username = "wrong")

    val someGuWid = "someGuWid"
    val someAuth = WirecardAuthorization(someGuWid, transactionId)
    val resultGuWid = "resultGuWid"

    val somePayment = successfulPayment

    def givenWirecardAuthorizationRequest = driver.aPreauthorizationRequest(wirecardTestCredentials,
      transactionId, testCreditCard, somePayment, testWirecardAddress)

    def givenWirecardCaptureRequest = driver.aCaptureRequest(wirecardTestCredentials, someAuth,
      somePayment.currencyAmount.amount)

    def givenWirecardPurchaseRequest = driver.aPurchaseRequest(wirecardTestCredentials,
      transactionId, testCreditCard, somePayment, testWirecardAddress)

    def givenWirecardVoidAuthRequest = driver.aVoidAuthorizationRequest(wirecardTestCredentials, someAuth)

    def hasMessage(message: String) =
      be_==("Connection attempt to localhost:10002 failed") ^^ ((e: Throwable) => e.getMessage)
  }
}
