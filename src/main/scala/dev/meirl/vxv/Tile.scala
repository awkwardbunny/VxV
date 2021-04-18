package dev.meirl.vxv

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters

class TileIO(implicit val p: Parameters) extends Bundle {
  val o = Output(UInt(p(XLEN).W))
}

class Tile(params: Parameters) extends Module {
  implicit val p = params
  val io = IO(new TileIO)

  io.o := 5.U

  val core = Module(new Core)
}
