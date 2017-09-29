package com.wix.pay.wirecard.testkit

import java.util.UUID

import com.wix.hoopoe.http.testkit.EmbeddedHttpProbe
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.Payment
import com.wix.pay.wirecard.http.WirecardRequestBuilder._
import com.wix.pay.wirecard.testkit.WirecardResponseBuilder._
import com.wix.pay.wirecard.{WirecardAddress, WirecardAuthorization, WirecardMerchant}
import spray.http.HttpHeaders.Authorization
import spray.http.{Uri, _}
import WirecardDriver._

import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Elem, Node, NodeSeq, XML}

class WirecardDriver(probe: EmbeddedHttpProbe, matchTransactionId: Boolean) {
  def this(port: Int, matchTransactionId: Boolean = true) = this(
    new EmbeddedHttpProbe(port, EmbeddedHttpProbe.NotFoundHandler),
    matchTransactionId
  )

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

  def anyPreauthorizationRequest() =
    new AnyWirecardRequest(PreAuthorizationFunction)

  def anyCaptureRequest() =
    new AnyWirecardRequest(CaptureFunction)

  def anyPurchaseRequest() =
    new AnyWirecardRequest(PurchaseFunction)

  def anyReversalRequest() =
    new AnyWirecardRequest(ReversalFunction)

  class AnyWirecardRequest(wirecardFunction: String, transactionId: String = UUID.randomUUID.toString) {
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

    protected def isAuthorized(headers: List[HttpHeader]): Boolean = true

    protected def isStubbedEntity(entity: HttpEntity): Boolean = true
  }

  abstract class FullfiledWirecardRequest(wirecardFunction: String,
                                          transactionId: String,
                                          credentials: WirecardMerchant,
                                          expectedXmlBody: Elem,
                                          wirecardAppCredentials: WirecardAppCredentials)
    extends AnyWirecardRequest(wirecardFunction, transactionId) {

    def isFailedWithWrongBusinessCaseSignature() =
      respondWithOk(wrongBusinessCaseSignature(credentials.businessCaseSignature))

    override protected def isStubbedEntity(entity: HttpEntity): Boolean =
      toXml(expectedXmlBody.toString) xml_== toXml(entity.asString)

    private def toXml(content: String): NodeSeq = {
      val xml = XML.loadString(content)
      if (!matchTransactionId) {
        new RuleTransformer(RemoveTransactionIdRule).transform(xml)
      } else {
        xml
      }
    }

    override protected def isAuthorized(headers: List[HttpHeader]): Boolean = headers.contains(
      Authorization(BasicHttpCredentials(wirecardAppCredentials.username, wirecardAppCredentials.password))
    )
  }

  case class PreAuthorizationRequest(credentials: WirecardMerchant,
                                     transactionId: String,
                                     creditCard: CreditCard,
                                     payment: Payment,
                                     address: WirecardAddress,
                                     wirecardAppCredentials: WirecardAppCredentials)
    extends FullfiledWirecardRequest(
      PreAuthorizationFunction,
      transactionId,
      credentials,
      createPreauthorizationRequest(transactionId, credentials, creditCard, payment, address), wirecardAppCredentials)

  case class CaptureRequest(credentials: WirecardMerchant,
                            auth: WirecardAuthorization,
                            amount: Double,
                            wirecardAppCredentials: WirecardAppCredentials)
    extends FullfiledWirecardRequest(
      CaptureFunction,
      auth.transactionId,
      credentials,
      createCaptureRequest(auth, credentials, amount), wirecardAppCredentials)

  case class PurchaseRequest(credentials: WirecardMerchant,
                             transactionId: String,
                             creditCard: CreditCard,
                             payment: Payment,
                             address: WirecardAddress,
                             wirecardAppCredentials: WirecardAppCredentials)
    extends FullfiledWirecardRequest(
      PurchaseFunction,
      transactionId,
      credentials,
      createPurchaseRequest(transactionId, credentials, creditCard, payment, address), wirecardAppCredentials)

  case class VoidAuthorizationRequest(credentials: WirecardMerchant,
                                      auth: WirecardAuthorization,
                                      wirecardAppCredentials: WirecardAppCredentials)
    extends FullfiledWirecardRequest(
      ReversalFunction,
      auth.transactionId,
      credentials,
      createReversalRequest(auth, credentials),
      wirecardAppCredentials)
}

object WirecardDriver {
  val PurchaseFunction = "FNC_CC_PURCHASE"
  val PreAuthorizationFunction = "FNC_CC_PREAUTHORIZATION"
  val CaptureFunction = "FNC_CC_CAPTURE_PREAUTHORIZATION"
  val ReversalFunction = "FNC_CC_REVERSAL"
}

private object RemoveTransactionIdRule extends RewriteRule {
  override def transform(n: Node): Seq[Node] = n match {
    case Elem(_, "TransactionID", _, _, _) => NodeSeq.Empty
    case x => x
  }
}