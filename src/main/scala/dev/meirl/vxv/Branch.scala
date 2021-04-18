package dev.meirl.vxv

import chipsalliance.rocketchip.config.Parameters
import chisel3._

object BR_COND extends Enumeration {
  type BR_COND = UInt
  val BR_NONE = 0.U(3.W)
  val BR_LTU  = 1.U(3.W)
  val BR_LT   = 2.U(3.W)
  val BR_EQ   = 3.U(3.W)
  val BR_GEU  = 4.U(3.W)
  val BR_GE   = 5.U(3.W)
  val BR_NE   = 6.U(3.W)
}

class BranchIO(implicit p: Parameters) extends Bundle {
  val xlen = p(XLEN)

  val rs1 = Input(UInt(xlen.W))
  val rs2 = Input(UInt(xlen.W))
  val sel = Input(UInt(3.W))
  val taken = Output(Bool())
}

import BR_COND._

class Branch(implicit p: Parameters) extends Module {
  val xlen = p(XLEN)
  val io = IO(new BranchIO)

  val diff = io.rs1 - io.rs2
  val neq  = diff.orR
  val eq   = !neq
  val isSameSign = io.rs1(xlen-1) === io.rs2(xlen-1)
  val lt   = Mux(isSameSign, diff(xlen-1), io.rs1(xlen-1))
  val ltu  = Mux(isSameSign, diff(xlen-1), io.rs2(xlen-1))
  val ge   = !lt
  val geu  = !ltu
  io.taken :=
    ((io.sel === BR_EQ) && eq) ||
      ((io.sel === BR_NE) && neq) ||
      ((io.sel === BR_LT) && lt) ||
      ((io.sel === BR_GE) && ge) ||
      ((io.sel === BR_LTU) && ltu) ||
      ((io.sel === BR_GEU) && geu)
}
