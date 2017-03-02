package com.wix.pay.wirecard.parsers
import com.wix.pay.wirecard.WirecardMerchant
import org.json4s.DefaultFormats
import org.json4s.native.Serialization

class JsonWirecardMerchantParser extends WirecardMerchantParser {
  private implicit val formats = DefaultFormats

  override def parse(merchantKey: String): WirecardMerchant =
    Serialization.read[WirecardMerchant](merchantKey)

  override def stringify(merchant: WirecardMerchant): String =
    Serialization.write(merchant)
}
