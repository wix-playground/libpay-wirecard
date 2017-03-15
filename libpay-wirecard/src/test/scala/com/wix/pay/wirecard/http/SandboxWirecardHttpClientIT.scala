package com.wix.pay.wirecard.http

import com.wix.pay.wirecard.{WirecardMerchant, WirecardAuthorization}
import com.wix.pay.wirecard.testkit.{WirecardAppCredentials, WirecardDriver}
import com.wix.pay.{PaymentErrorException, PaymentRejectedException}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.{BeforeEach, Scope}

class SandboxWirecardHttpClientIT extends SpecWithJUnit with WirecardHttpClientTestSupport with BeforeEach{

  val driver = new WirecardDriver(port = 10001)
  override val wirecardTestCredentials = WirecardMerchant("56501", testMode = true)

  val liveAppCredentials = WirecardAppCredentials(username = "liveUsername", password = "livePassword")
  val testAppCredentials = WirecardAppCredentials(username = "testUsername", password = "testPassword")

  val appCredentials = testAppCredentials

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

    "fail with wrong businessCaseSignature" in new Ctx {
      givenWirecardAuthorizationRequest isFailedWithWrongBusinessCaseSignature()
      preauthorize(somePayment) must beFailedTry(PaymentErrorException(s"Wrong BC Advice: Business Case Signature " +
        s"'${wirecardTestCredentials.businessCaseSignature}' is not accepted"))
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
      preauthorize(somePayment, wrongMerchantCredentials) must beFailedTry(PaymentErrorException("HTTP error. Status: 401"))
    }
  }

  step {
    driver.stop()
  }

  override protected def before: Any = driver.reset()

  trait Ctx extends Scope {

    val liveMerchantCredentials = wirecardTestCredentials.copy(testMode = false)
    val wrongMerchantCredentials = wirecardTestCredentials.copy(businessCaseSignature = "wrong")

    val someGuWid = "someGuWid"
    val someAuth = WirecardAuthorization(someGuWid, transactionId)
    val resultGuWid = "resultGuWid"

    val somePayment = successfulPayment

    def givenWirecardAuthorizationRequest = driver.aPreauthorizationRequest(wirecardTestCredentials, appCredentials,
      transactionId, testCreditCard, somePayment, testWirecardAddress)

    def givenWirecardCaptureRequest = driver.aCaptureRequest(wirecardTestCredentials, appCredentials, someAuth,
      somePayment.currencyAmount.amount)

    def givenWirecardPurchaseRequest = driver.aPurchaseRequest(wirecardTestCredentials, appCredentials,
      transactionId, testCreditCard, somePayment, testWirecardAddress)

    def givenWirecardVoidAuthRequest = driver.aVoidAuthorizationRequest(wirecardTestCredentials, appCredentials, someAuth)

    def hasMessage(message: String) =
      be_==("Connection attempt to localhost:10002 failed") ^^ ((e: Throwable) => e.getMessage)
  }

  val httpClient = new SprayWirecardHttpClient(WirecardSettings(
    liveSettings = WirecardModeSettings(url = "http://localhost:10002", username = liveAppCredentials.username, password = liveAppCredentials.password),
    testSettings = WirecardModeSettings(url = "http://localhost:10001", username = testAppCredentials.username, password = testAppCredentials.password)))

}

class LiveWirecardHttpClientIT extends SandboxWirecardHttpClientIT {
  override val driver = new WirecardDriver(port = 10002)
  override val wirecardTestCredentials = WirecardMerchant("56501", testMode = false)

  override val appCredentials = liveAppCredentials
}