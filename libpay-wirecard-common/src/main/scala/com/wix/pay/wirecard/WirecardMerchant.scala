package com.wix.pay.wirecard

case class WirecardMerchant(username: String, password: String, businessCaseSignature: String, testMode: Boolean = false)
