package com.wix.pay.wirecard.testkit

import com.wix.hoopoe.http.testkit.EmbeddedHttpProbe
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.Payment
import com.wix.pay.wirecard.http.WirecardRequestBuilder._
import com.wix.pay.wirecard.testkit.WirecardResponseBuilder._
import com.wix.pay.wirecard.{WirecardAddress, WirecardAuthorization, WirecardMerchant}
import spray.http.HttpHeaders.Authorization
import spray.http.{Uri, _}

import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Elem, Node, NodeSeq, XML}

class WirecardDriver(port: Int, matchTransactionId: Boolean = true) {
  private val probe = new EmbeddedHttpProbe(port,
    {case HttpRequest(_, _, _, _, _) => HttpResponse(StatusCodes.Unauthorized)})

  def reset(): Unit = probe.reset()

  def start(): Unit = probe.doStart()

  def stop(): Unit = probe.doStop()

  def aPreauthorizationRequest(merchantCredentials: WirecardMerchant,
                               appCredentials: WirecardAppCredentials,
                               transactionId: String,
                               creditCard: CreditCard,
                               payment: Payment,
                               address: WirecardAddress) =
    PreAuthorizationRequest(merchantCredentials, transactionId, creditCard, payment, address, appCredentials)

  def aCaptureRequest(merchantCredentials: WirecardMerchant,
                      appCredentials: WirecardAppCredentials,
                      auth: WirecardAuthorization,
                      amount: Double) =
    CaptureRequest(merchantCredentials, auth, amount, appCredentials)

  def aPurchaseRequest(merchantCredentials: WirecardMerchant,
                       appCredentials: WirecardAppCredentials,
                       transactionId: String,
                       creditCard: CreditCard,
                       payment: Payment,
                       address: WirecardAddress) =
    PurchaseRequest(merchantCredentials, transactionId, creditCard, payment, address, appCredentials)

  def aVoidAuthorizationRequest(merchantCredentials: WirecardMerchant,
                                appCredentials: WirecardAppCredentials,
                      auth: WirecardAuthorization) =
    VoidAuthorizationRequest(merchantCredentials, auth, appCredentials)


  abstract class WirecardRequest(credentials: WirecardMerchant, transactionId: String,
                                 wirecardFunction: String, expectedXmlBody: Elem, wirecardAppCredentials: WirecardAppCredentials) {
    protected val defaultNumericValue = "1234567"

    def isFailedOnServer() =
      respondWith(StatusCodes.InternalServerError, serverFailureResponse.toString)

    def isRejectedWith(error: String, advice: String = ""): Unit = {
      val payload = response(wirecardFunction, failedBody(transactionId, defaultNumericValue, true, error, advice))
      respondWithOk(payload)
    }

    def isFailedWith(error: String, advice: String = ""): Unit = {
      val payload = response(wirecardFunction, failedBody(transactionId, defaultNumericValue, false, error, advice))
      respondWithOk(payload)
    }

    def isValidWith(guWid: String): Unit = {
      val payload = response(wirecardFunction, validBody(transactionId, guWid))
      respondWithOk(payload)
    }

    protected def respondWith(status: StatusCode, content: String): Unit = {
      probe.handlers += {
        case HttpRequest(HttpMethods.POST, requestPath, headers, entity, _)
          if requestPath.path == Uri("/").path &&
            isAuthorized(headers) &&
            isStubbedEntity(entity) =>
          HttpResponse(status = status, entity = content)
      }
    }

    protected def respondWithOk(content: Any): Unit = {
      respondWith(StatusCodes.OK, content.toString)
    }

    private def isStubbedEntity(entity: HttpEntity): Boolean =
      toXml(expectedXmlBody.toString) xml_== toXml(entity.asString)

    private def toXml(content: String): NodeSeq = {
      val xml = XML.loadString(content)
      if (!matchTransactionId) {
        new RuleTransformer(RemoveTransactionIdRule).transform(xml)
      } else {
        xml
      }
    }

    private def isAuthorized(headers: List[HttpHeader]): Boolean = headers.contains(
      Authorization(BasicHttpCredentials(wirecardAppCredentials.username, wirecardAppCredentials.password))
    )
  }

  case class PreAuthorizationRequest(credentials: WirecardMerchant,
                                     transactionId: String,
                                     creditCard: CreditCard,
                                     payment: Payment,
                                     address: WirecardAddress,
                                     wirecardAppCredentials: WirecardAppCredentials)
    extends WirecardRequest(
      credentials,
      transactionId,
      "FNC_CC_PREAUTHORIZATION",
      createPreauthorizationRequest(transactionId, credentials, creditCard, payment, address), wirecardAppCredentials)

  case class CaptureRequest(credentials: WirecardMerchant,
                            auth: WirecardAuthorization,
                            amount: Double,
                            wirecardAppCredentials: WirecardAppCredentials)
    extends WirecardRequest(
      credentials,
      auth.transactionId,
      "FNC_CC_CAPTURE_PREAUTHORIZATION",
      createCaptureRequest(auth, credentials, amount), wirecardAppCredentials)

  case class PurchaseRequest(credentials: WirecardMerchant,
                             transactionId: String,
                             creditCard: CreditCard,
                             payment: Payment,
                             address: WirecardAddress,
                             wirecardAppCredentials: WirecardAppCredentials)
    extends WirecardRequest(
      credentials,
      transactionId,
      "FNC_CC_PURCHASE",
      createPurchaseRequest(transactionId, credentials, creditCard, payment, address), wirecardAppCredentials)

  case class VoidAuthorizationRequest(credentials: WirecardMerchant,
                                      auth: WirecardAuthorization,
                                      wirecardAppCredentials: WirecardAppCredentials)
    extends WirecardRequest(
      credentials,
      auth.transactionId,
      "FNC_CC_REVERSAL",
      createReversalRequest(auth, credentials),
      wirecardAppCredentials)
}

private object RemoveTransactionIdRule extends RewriteRule {
  override def transform(n: Node): Seq[Node] = n match {
    case Elem(_, "TransactionID", _, _, _) => NodeSeq.Empty
    case x => x
  }
}