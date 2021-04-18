package dev.meirl.vxv

import chisel3.util.MuxLookup
import chisel3.{Mem, Mux, when, _}
import chipsalliance.rocketchip.config.Parameters

object ALU_OP extends Enumeration {
  type ALU_OP = UInt
  val ALU_ADD    = 0.U(4.W)
  val ALU_SUB    = 1.U(4.W)
  val ALU_AND    = 2.U(4.W)
  val ALU_OR     = 3.U(4.W)
  val ALU_XOR    = 4.U(4.W)
  val ALU_SLT    = 5.U(4.W)
  val ALU_SLL    = 6.U(4.W)
  val ALU_SLTU   = 7.U(4.W)
  val ALU_SRL    = 8.U(4.W)
  val ALU_SRA    = 9.U(4.W)
  val ALU_COPY_A = 10.U(4.W)
  val ALU_COPY_B = 11.U(4.W)
  val ALU_NOP    = 12.U(4.W)
}

class ALUIO(implicit val p: Parameters) extends Bundle {
  val xlen = p(XLEN)

  val A = Input(UInt(xlen.W))
  val B = Input(UInt(xlen.W))
  val op = Input(UInt(4.W))

  val out = Output(UInt(xlen.W))
}

import ALU_OP._

class ALU(implicit p: Parameters) extends Module {
  val io = IO(new ALUIO)
  val shamt = io.B(4,0).asUInt

  io.out := MuxLookup(io.op, io.B, Seq(
    ALU_ADD    -> (io.A + io.B),
    ALU_SUB    -> (io.A - io.B),
    ALU_SRA    -> (io.A.asSInt >> shamt).asUInt,
    ALU_SRL    -> (io.A >> shamt),
    ALU_SLL    -> (io.A << shamt),
    ALU_SLT    -> (io.A.asSInt < io.B.asSInt),
    ALU_SLTU   -> (io.A < io.B),
    ALU_AND    -> (io.A & io.B),
    ALU_OR     -> (io.A | io.B),
    ALU_XOR    -> (io.A ^ io.B),
    ALU_COPY_A -> io.A,
    ALU_NOP    -> 0.U
  ))
}
