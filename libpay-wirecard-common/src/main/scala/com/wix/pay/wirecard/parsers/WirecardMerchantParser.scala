package com.wix.pay.wirecard.parsers

import com.wix.pay.wirecard.WirecardMerchant

trait WirecardMerchantParser {
  def parse(merchantKey: String): WirecardMerchant
  def stringify(merchant: WirecardMerchant): String
}
