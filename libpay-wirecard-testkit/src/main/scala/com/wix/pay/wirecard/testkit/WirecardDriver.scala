package com.wix.pay.wirecard.testkit

import java.util.UUID

import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials}
import com.wix.e2e.http.api.StubWebServer
import com.wix.e2e.http.client.extractors.HttpMessageExtractors._
import com.wix.e2e.http.server.WebServerFactory.aStubWebServer
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.Payment
import com.wix.pay.wirecard.http.WirecardRequestBuilder._
import com.wix.pay.wirecard.testkit.WirecardDriver._
import com.wix.pay.wirecard.testkit.WirecardResponseBuilder._
import com.wix.pay.wirecard.{WirecardAddress, WirecardAuthorization, WirecardMerchant}

import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Elem, Node, NodeSeq, XML}


class WirecardDriver(server: StubWebServer, matchTransactionId: Boolean) {
  def this(port: Int, matchTransactionId: Boolean = true) = this(aStubWebServer.onPort(port).build, matchTransactionId)

  def start(): Unit = server.start()
  def stop(): Unit = server.stop()
  def reset(): Unit = server.replaceWith()

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

    def getsFailedOnServer(): Unit = respondWith(StatusCodes.InternalServerError, serverFailureResponse.toString)

    def getsRejectedWith(error: String, advice: String, transactionId: Option[String] = None, guWid: Option[String] = None) {
      val payload = response(
        wirecardFunction,
        failedBody(transactionId, guWid, isRejected = true, error, advice))
      respondWithOk(payload)
    }

    def getsFailedWith(error: String, advice: String, transactionId: Option[String] = None, guWid: Option[String] = None): Unit = {
      val payload = response(
        wirecardFunction,
        failedBody(transactionId, guWid, isRejected = false, error, advice))
      respondWithOk(payload)
    }

    def getsValidWith(guWid: String): Unit = {
      val payload = response(wirecardFunction, validBody(transactionId, guWid))
      respondWithOk(payload)
    }

    protected def respondWith(status: StatusCode, content: String): Unit = {
      server.appendAll {
        case HttpRequest(HttpMethods.POST, Path("/"), headers, entity, _)
          if isAuthorized(headers) &&
            isStubbedEntity(entity) =>
          HttpResponse(status = status, entity = content)
      }
    }

    protected def respondWithOk(content: Any): Unit = respondWith(StatusCodes.OK, content.toString)
    protected def isAuthorized(headers: Seq[HttpHeader]): Boolean = true
    protected def isStubbedEntity(entity: HttpEntity): Boolean = true
  }

  abstract class FullfiledWirecardRequest(wirecardFunction: String,
                                          transactionId: String,
                                          credentials: WirecardMerchant,
                                          expectedXmlBody: Elem,
                                          wirecardAppCredentials: WirecardAppCredentials)
    extends AnyWirecardRequest(wirecardFunction, transactionId) {

    def getsFailedWithWrongBusinessCaseSignature(): Unit =
      respondWithOk(wrongBusinessCaseSignature(credentials.businessCaseSignature))

    override protected def isStubbedEntity(entity: HttpEntity): Boolean =
      toXml(expectedXmlBody.toString) xml_== toXml(entity.extractAsString)

    private def toXml(content: String): NodeSeq = {
      val xml = XML.loadString(content)
      if (!matchTransactionId) {
        new RuleTransformer(RemoveTransactionIdRule).transform(xml)
      } else {
        xml
      }
    }

    override protected def isAuthorized(headers: Seq[HttpHeader]): Boolean = headers.contains(
      Authorization(BasicHttpCredentials(wirecardAppCredentials.username, wirecardAppCredentials.password)))
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
