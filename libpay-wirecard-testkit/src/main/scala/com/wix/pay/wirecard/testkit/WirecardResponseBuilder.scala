package com.wix.pay.wirecard.testkit

import scala.xml.Elem

object WirecardResponseBuilder {

  def response(function: String, body: Elem) =
    <WIRECARD_BXML xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance" xsi:noNamespaceSchemaLocation="wirecard.xsd">
      <W_RESPONSE>
        <W_JOB>
          {body.copy(label = function)}
        </W_JOB>
      </W_RESPONSE>
    </WIRECARD_BXML>

  def validBody(transactionId: String, guWid: String) =
    <FNC_CC_BODY>
      <CC_TRANSACTION>
        <TransactionID>{transactionId}</TransactionID>
        <PROCESSING_STATUS>
          <GuWID>{guWid}</GuWID>
          <AuthorizationCode>639452</AuthorizationCode>
          <StatusType>INFO</StatusType>
          <FunctionResult>PENDING</FunctionResult>
          <TimeStamp>2017-03-01 01:14:00</TimeStamp>
        </PROCESSING_STATUS>
      </CC_TRANSACTION>
    </FNC_CC_BODY>

  def failedBody(transactionId: String, guWid: String, isRejected: Boolean, error: String, advice: String) =
    <FNC_CC_BODY>
      <CC_TRANSACTION>
        <TransactionID>{transactionId}</TransactionID>
        <PROCESSING_STATUS>
          <GuWID>{guWid}</GuWID>
          <AuthorizationCode>639452</AuthorizationCode>
          <StatusType>INFO</StatusType>
          <FunctionResult>NOK</FunctionResult>
          <ERROR>
            <Type>{if (isRejected) "REJECTED" else "ERROR"}</Type>
            <Number>05</Number>
            <Message>{error}</Message>
            <Advice>{advice}</Advice>
          </ERROR>
          <TimeStamp>2017-03-01 01:14:00</TimeStamp>
        </PROCESSING_STATUS>
      </CC_TRANSACTION>
    </FNC_CC_BODY>

  def serverFailureResponse =
    <pre>This is an error page</pre>
}
