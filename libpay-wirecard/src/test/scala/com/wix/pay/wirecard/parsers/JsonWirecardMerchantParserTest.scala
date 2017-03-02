package com.wix.pay.wirecard.parsers

import com.wix.pay.wirecard.WirecardMerchant
import org.specs2.mutable.SpecWithJUnit

class JsonWirecardMerchantParserTest extends SpecWithJUnit {
  val parser = new JsonWirecardMerchantParser

  "JsonWirecardMerchantParser" should {
    "parse credentials from string" in {
      val credentials =
        """
          {
            "username" : "username",
            "password" : "password",
            "businessCaseSignature" : "businessCaseSignature",
            "testMode" : true
          }
        """
      parser.parse(credentials) mustEqual WirecardMerchant("username", "password", "businessCaseSignature", true)
    }

    "parse credentials from string without mode" in {
      val credentials =
        """
          {
            "username" : "username",
            "password" : "password",
            "businessCaseSignature" : "businessCaseSignature"
          }
        """
      parser.parse(credentials) mustEqual WirecardMerchant("username", "password", "businessCaseSignature", false)
    }

    "stringify credentials" in {
      val result = parser.stringify(WirecardMerchant("username", "password", "businessCaseSignature", false))
      result mustEqual
        """{"username":"username","password":"password","businessCaseSignature":"businessCaseSignature","testMode":false}"""
    }
  }
}
