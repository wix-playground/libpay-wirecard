package com.wix.pay.wirecard.http

import akka.actor.ActorSystem
import com.wix.pay.{PaymentErrorException, PaymentException, PaymentRejectedException}
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.Payment
import com.wix.pay.wirecard.http.WirecardRequestBuilder._
import com.wix.pay.wirecard.{WirecardAddress, WirecardAuthorization, WirecardMerchant}
import spray.http._
import spray.client.pipelining._
import spray.http.HttpHeaders.Authorization

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Try
import scala.xml.{Elem, XML}

class SprayWirecardHttpClient(wirecardUrls: WirecardUrls) extends WirecardHttpClient {
  implicit val system = ActorSystem()

  override def purchase(credentials: WirecardMerchant, transactionId: String, creditCard: CreditCard,
                        payment: Payment, address: WirecardAddress): Try[String] = {
    postRequest(credentials, createPurchaseRequest(
      transactionId,
      credentials,
      creditCard,
      payment,
      address
    )).map(_.guWid)
  }

  override def preauthorize(credentials: WirecardMerchant, transactionId: String, creditCard: CreditCard,
                            payment: Payment, address: WirecardAddress): Try[WirecardAuthorization] = {
    postRequest(credentials, createPreauthorizationRequest(
      transactionId,
      credentials,
      creditCard,
      payment,
      address
    ))
  }

  override def capture(credentials: WirecardMerchant, authorization: WirecardAuthorization, amount: Double): Try[String] =
    postRequest(credentials, createCaptureRequest(authorization, credentials, amount)).map(_.guWid)

  override def voidPreauthorization(credentials: WirecardMerchant,
                                    authorization: WirecardAuthorization): Try[String] =
    postRequest(credentials, createReversalRequest(authorization, credentials)).map(_.guWid)


  private def postRequest(credentials: WirecardMerchant, payload: Elem) = {
    import system.dispatcher
    val pipeline: HttpRequest => Future[HttpResponse] = sendReceive

    val request = Post(gatewayUrlFor(credentials), payload)
          .withHeaders(Authorization(BasicHttpCredentials(credentials.username, credentials.password)))
    Try {
      val rawResponse = Await.result(pipeline(request), 30.seconds)
      if (!rawResponse.status.isSuccess) {
        throw PaymentErrorException(s"HTTP error. Status: ${rawResponse.status.intValue}")
      }

      parseWirecardResponse(XML.loadString(rawResponse.entity.asString))
    } recover {
      case e: PaymentException => throw e
      case e => throw PaymentErrorException(e.getMessage, e)
    }
  }

  private def parseWirecardResponse(response: Elem) = {
    val result = (response \\ "FunctionResult").text

    if (result == "NOK") {
      val error = response \\ "ERROR"
      val message = (error \ "Message").text + " Advice: " + (error \ "Advice").text
      if ((error \ "Type").text == "REJECTED")
        throw PaymentRejectedException(message)
      else
        throw PaymentErrorException(message)
    }

    val guWid = (response \\ "GuWID").text
    val transactionId = (response \\ "TransactionID").text
    WirecardAuthorization(guWid, transactionId)
  }

  private def gatewayUrlFor(credentials: WirecardMerchant): String =
    if (credentials.testMode) wirecardUrls.testUrl else wirecardUrls.liveUrl
}
