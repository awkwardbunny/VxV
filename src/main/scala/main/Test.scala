package main

import chisel3._
import dev.meirl.vxv.XLEN
import chipsalliance.rocketchip.config.Parameters

class Test(params: Parameters) extends Module {
  val io = IO(new Bundle {
    val btn = Input(UInt(params(XLEN).W))
    val led = Output(UInt(params(XLEN).W))
  })
  val v = Reg(UInt(params(XLEN).W))
  io.led := v
  when(true.B){
    v := io.btn
  }
}