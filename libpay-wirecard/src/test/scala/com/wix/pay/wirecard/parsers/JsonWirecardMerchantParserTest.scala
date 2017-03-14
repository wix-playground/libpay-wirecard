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
            "businessCaseSignature" : "businessCaseSignature",
            "testMode" : true
          }
        """
      parser.parse(credentials) mustEqual WirecardMerchant("businessCaseSignature", true)
    }

    "parse credentials from string without mode" in {
      val credentials =
        """
          {
            "businessCaseSignature" : "businessCaseSignature"
          }
        """
      parser.parse(credentials) mustEqual WirecardMerchant("businessCaseSignature", false)
    }

    "stringify credentials" in {
      val result = parser.stringify(WirecardMerchant("businessCaseSignature", false))
      result mustEqual
        """{"businessCaseSignature":"businessCaseSignature","testMode":false}"""
    }
  }
}
