package dev.meirl.vxv

import chipsalliance.rocketchip.config.{Config,Field}

case object XLEN extends Field[Int]

class VxVConfig extends Config((site, here, up) => {
  case XLEN => 32
  }
)
