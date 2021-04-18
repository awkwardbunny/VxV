package dev.meirl.vxv

import chisel3._
import chipsalliance.rocketchip.config.Parameters

class CoreIO(implicit val p: Parameters) extends Bundle

class Core(implicit val p: Parameters) extends Module {
  val io = IO(new CoreIO)
  val regFile = Module(new RegFile)
  val alu = Module(new ALU)
  val immGen = Module(new ImmGen)

  val counter = Reg(UInt(p(XLEN).W))
  when(true.B){
    counter := counter + 1.U
  }

  regFile.io.raddr1 := 0.U
  regFile.io.raddr2 := 0.U
  regFile.io.waddr  := 0.U
  regFile.io.wdata  := counter
  regFile.io.wen    := true.B
}
