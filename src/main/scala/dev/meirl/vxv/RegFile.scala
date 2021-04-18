package dev.meirl.vxv

import chisel3.{Mem, Mux, when, _}
import chipsalliance.rocketchip.config.Parameters

class RegFileIO(implicit val p: Parameters) extends Bundle {
  val xlen = p(XLEN)

  val raddr1 = Input(UInt(5.W))
  val raddr2 = Input(UInt(5.W))
  val rdata1 = Output(UInt(xlen.W))
  val rdata2 = Output(UInt(xlen.W))

  val wen = Input(Bool())
  val waddr = Input(UInt(5.W))
  val wdata = Input(UInt(xlen.W))
}

class RegFile(implicit p: Parameters) extends Module {
  val io = IO(new RegFileIO)
  val regs = Mem(32, UInt(p(XLEN).W))

  io.rdata1 := Mux(io.raddr1.orR, regs(io.raddr1), 0.U)
  io.rdata2 := Mux(io.raddr2.orR, regs(io.raddr2), 0.U)
  when(io.wen & io.waddr.orR) {
    regs(io.waddr) := io.wdata
  }
}
