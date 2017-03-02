package com.wix.pay.wirecard.http

import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.Payment
import com.wix.pay.wirecard.{WirecardAddress, WirecardAuthorization, WirecardMerchant}

object WirecardRequestBuilder {

  def createPreauthorizationRequest(transactionId: String, credentials: WirecardMerchant,
                                    creditCard: CreditCard, payment: Payment, address: WirecardAddress) =
    <WIRECARD_BXML xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance">
      <W_REQUEST>
        <W_JOB>
          <BusinessCaseSignature>{credentials.businessCaseSignature}</BusinessCaseSignature>
          <FNC_CC_PREAUTHORIZATION>
            {fillInitialTransaction(transactionId, credentials, creditCard, payment, address)}
          </FNC_CC_PREAUTHORIZATION>
        </W_JOB>
      </W_REQUEST>
    </WIRECARD_BXML>

  def createCaptureRequest(auth: WirecardAuthorization, credentials: WirecardMerchant, amount: Double) =
      <WIRECARD_BXML xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance">
        <W_REQUEST>
          <W_JOB>
            <BusinessCaseSignature>{credentials.businessCaseSignature}</BusinessCaseSignature>
            <FNC_CC_CAPTURE_PREAUTHORIZATION>
              <CC_TRANSACTION>
                <Amount minorunits="2" action="validate">{formatAmount(amount)}</Amount>
                <TransactionID>{auth.transactionId}</TransactionID>
                <GuWID>{auth.guWid}</GuWID>
              </CC_TRANSACTION>
            </FNC_CC_CAPTURE_PREAUTHORIZATION>
          </W_JOB>
        </W_REQUEST>
      </WIRECARD_BXML>

  def createPurchaseRequest(transactionId: String, credentials: WirecardMerchant,
                            creditCard: CreditCard, payment: Payment, address: WirecardAddress) =
    <WIRECARD_BXML xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance">
      <W_REQUEST>
        <W_JOB>
          <BusinessCaseSignature>{credentials.businessCaseSignature}</BusinessCaseSignature>
          <FNC_CC_PURCHASE>
            {fillInitialTransaction(transactionId, credentials, creditCard, payment, address)}
          </FNC_CC_PURCHASE>
        </W_JOB>
      </W_REQUEST>
    </WIRECARD_BXML>

  def createReversalRequest(auth: WirecardAuthorization, credentials: WirecardMerchant) =
    <WIRECARD_BXML xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance">
      <W_REQUEST>
        <W_JOB>
          <BusinessCaseSignature>{credentials.businessCaseSignature}</BusinessCaseSignature>
          <FNC_CC_REVERSAL>
            {fillAuthorizedTransaction(auth, credentials)}
          </FNC_CC_REVERSAL>
        </W_JOB>
      </W_REQUEST>
    </WIRECARD_BXML>


  private def fillInitialTransaction(transactionId: String, credentials: WirecardMerchant,
                                      creditCard: CreditCard, payment: Payment, address: WirecardAddress) =
    <CC_TRANSACTION>
      <TransactionID>{transactionId}</TransactionID>
      <Amount minorunits="2" action="validate">{formatAmount(payment.currencyAmount.amount)}</Amount>
      <Currency>{payment.currencyAmount.currency}</Currency>
      <CountryCode>US</CountryCode>
      <CREDIT_CARD_DATA>
        <CreditCardNumber>{creditCard.number}</CreditCardNumber>
        <ExpirationYear>{creditCard.expiration.year}</ExpirationYear>
        <ExpirationMonth>{creditCard.expiration.month formatted "%02d"}</ExpirationMonth>
        <CardHolderName>{creditCard.holderName.get}</CardHolderName>
        <CVC2>{creditCard.csc.get}</CVC2>
      </CREDIT_CARD_DATA>
      {if (!address.isEmpty) fillAddress(address) else null}
    </CC_TRANSACTION>

  private def fillAuthorizedTransaction(auth: WirecardAuthorization, credentials: WirecardMerchant) =
    <CC_TRANSACTION>
      <TransactionID>{auth.transactionId}</TransactionID>
      <GuWID>{auth.guWid}</GuWID>
    </CC_TRANSACTION>

  private def fillAddress(address: WirecardAddress) =
    <CORPTRUSTCENTER_DATA>
      <ADDRESS>
        <FirstName>{address.firstName}</FirstName>
        <LastName>{address.lastName}</LastName>
        <Address1>{address.address1}</Address1>
        <Address2>{address.address2}</Address2>
        <City>{address.city}</City>
        <ZipCode>{address.zipCode}</ZipCode>
        <State>{address.state}</State>
        <Country>{address.country}</Country>
        <Email>{address.email}</Email>
      </ADDRESS>
    </CORPTRUSTCENTER_DATA>

  private def formatAmount(amount: Double): Long =
    (amount * 100).toLong
}
