package dev.meirl.vxv

import chisel3._
import chisel3.util.{Cat, MuxLookup}
import chipsalliance.rocketchip.config.Parameters

class ImmGenIO(implicit p: Parameters) extends Bundle {
  val xlen = p(XLEN)

  val inst = Input(UInt(xlen.W))
  val sel = Input(UInt(3.W))
  val imm = Output(UInt(xlen.W))
}

class ImmGen(implicit p: Parameters) extends Module {
  val io = IO(new ImmGenIO)

  val Iimm = io.inst(31, 20).asSInt
  io.imm := MuxLookup(io.sel, Iimm & -2.S, Seq(
    InstructionType.I -> Iimm,
    InstructionType.S -> Cat(io.inst(31, 25), io.inst(11,7)).asSInt,
    InstructionType.B -> Cat(io.inst(31), io.inst(7), io.inst(30, 25), io.inst(11, 8), 0.U(1.W)).asSInt,
    InstructionType.U -> Cat(io.inst(31, 12), 0.U(12.W)).asSInt,
    InstructionType.J -> Cat(io.inst(31), io.inst(19, 12), io.inst(20), io.inst(30, 25), io.inst(24, 21), 0.U(1.W)).asSInt,
    InstructionType.Z -> io.inst(19, 15).zext
  )).asUInt
}
