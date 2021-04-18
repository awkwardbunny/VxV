package dev.meirl.vxv

import chisel3._
import chisel3.util.BitPat
import dev.meirl.vxv.ALU_OP._
import dev.meirl.vxv.InstructionType.InstructionType

object InstructionType extends Enumeration {
  type InstructionType = UInt
  val R = 0.U(3.W)
  val I = 1.U(3.W)
  val S = 2.U(3.W)
  val B = 3.U(3.W)
  val U = 4.U(3.W)
  val J = 5.U(3.W)
  val Z = 6.U(3.W)
}

case class Instruction(bitpat: BitPat, itype: InstructionType, aluOp: ALU_OP)

object Instructions {
  // Register-Immediate Instructions
  val ADDI  = Instruction.I(0x13, 0x0, ALU_ADD)
  val SLTI  = Instruction.I(0x13, 0x2, ALU_SLT)
  val SLTIU = Instruction.I(0x13, 0x3, ALU_SLTU)
  val XORI  = Instruction.I(0x13, 0x4, ALU_XOR)
  val ORI   = Instruction.I(0x13, 0x6, ALU_OR)
  val ANDI  = Instruction.I(0x13, 0x7, ALU_AND)

//  val SLLI  = Instruction(0x13, 0x1, 0x00, InstructionType.I)
//  val SRLI  = Instruction(0x13, 0x5, 0x00, InstructionType.I)
//  val SRAI  = Instruction(0x13, 0x5, 0x20, InstructionType.I)
  val SLLI = Instruction("b0000000??????????001?????0010011", InstructionType.I, ALU_SLL)
  val SRLI = Instruction("b0000000??????????101?????0010011", InstructionType.I, ALU_SRL)
  val SRAI = Instruction("b0000000??????????101?????0010011", InstructionType.I, ALU_SRA)

  val LUI   = Instruction.U(0x37, ALU_COPY_B)
  val AUIPC = Instruction.U(0x17, ALU_ADD)

  // Register-Register Instructions
  val ADD  = Instruction.R(0x33, 0x0, 0x00, ALU_ADD)
  val SUB  = Instruction.R(0x33, 0x0, 0x20, ALU_SUB)
  val SLL  = Instruction.R(0x33, 0x1, 0x00, ALU_SLL)
  val SLT  = Instruction.R(0x33, 0x2, 0x00, ALU_SLT)
  val SLTU = Instruction.R(0x33, 0x3, 0x00, ALU_SLTU)
  val XOR  = Instruction.R(0x33, 0x4, 0x00, ALU_XOR)
  val SRL  = Instruction.R(0x33, 0x5, 0x00, ALU_SRL)
  val SRA  = Instruction.R(0x33, 0x5, 0x20, ALU_SRA)
  val OR   = Instruction.R(0x33, 0x6, 0x00, ALU_OR)
  val AND  = Instruction.R(0x33, 0x7, 0x00, ALU_AND)

  // Unconditional Jumps
  val JAL   = Instruction.J(0x6F, ALU_ADD)
  val JALR  = Instruction.I(0x67, 0x0, ALU_ADD)

  // Conditional Branches
  val BEQ  = Instruction.B(0x63, 0x0, ALU_ADD)
  val BNE  = Instruction.B(0x63, 0x1, ALU_ADD)
  val BLT  = Instruction.B(0x63, 0x4, ALU_ADD)
  val BGE  = Instruction.B(0x63, 0x5, ALU_ADD)
  val BLTU = Instruction.B(0x63, 0x6, ALU_ADD)
  val BGEU = Instruction.B(0x63, 0x7, ALU_ADD)

  // Load and Store Instructions
  val LB  = Instruction.I(0x03, 0x0, ALU_ADD)
  val LH  = Instruction.I(0x03, 0x1, ALU_ADD)
  val LW  = Instruction.I(0x03, 0x2, ALU_ADD)
  val LBU = Instruction.I(0x03, 0x4, ALU_ADD)
  val LHU = Instruction.I(0x03, 0x5, ALU_ADD)
  val SB  = Instruction.S(0x23, 0x0, ALU_ADD)
  val SH  = Instruction.S(0x23, 0x1, ALU_ADD)
  val SW  = Instruction.S(0x23, 0x2, ALU_ADD)

  // Fence
//  val FENCE = Instruction.I(0x0F, 0x0, ALU_NOP)
//  val FENCE_I = Instruction.I(0x0F, 0x1, ALU_NOP)

  // CSR
  val CSRRW  = Instruction.I(0x73, 0x1, ALU_COPY_A)
  val CSRRS  = Instruction.I(0x73, 0x2, ALU_COPY_A)
  val CSRRC  = Instruction.I(0x73, 0x3, ALU_COPY_A)
  val CSRRWI  = Instruction.Z(0x73, 0x5, ALU_NOP)
  val CSRRSI  = Instruction.Z(0x73, 0x6, ALU_NOP)
  val CSRRCI  = Instruction.Z(0x73, 0x7, ALU_NOP)

  // Environment Call and Breakpoints
  val ECALL  = Instruction("b00000000000000000000000001110011", InstructionType.I, ALU_NOP)
  val EBREAK = Instruction("b00000000000100000000000001110011", InstructionType.I, ALU_NOP)

  def getAllInstructionPatterns() = {
    import scala.reflect.runtime.universe._
    val rm = scala.reflect.runtime.currentMirror
    val accessors = rm.classSymbol(Instructions.getClass).toType.members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic => (m.name.toString, m)
    }

    val instanceMirror = rm.reflect(Instructions)
    accessors.map(acc => (acc._1 -> bitpatToString(instanceMirror.reflectMethod(acc._2)().asInstanceOf[Instruction].bitpat))).toList.reverse
  }

  private def bitpatToString(bp: BitPat): String = {
    val value = bp.value
    val mask = bp.mask
    var output = "b"
    for(i <- 1 to bp.getWidth) output = output + (if((mask & (1<<(i-1))) == 0) "?" else (if((value & (1<<(i-1))) == 0) "0" else "1"))
    output.reverse
  }
}

object Instruction {
  private implicit class BinaryNumber(val number: Int) extends AnyVal {
    def bitpat(width: Int) = {
      require(number < (1<<width))
      val bin = number.toBinaryString
      "0"*(width - bin.length) + bin
    }
  }

  def apply(opcode: Int, funct3: Int, funct7: Int, itype: InstructionType, aluOp: ALU_OP): Instruction = {
    require(opcode < (1<<7), s"""OpCode must be shorter than 8 bits. Given: "${opcode.toBinaryString}" """)
    require(funct3 < (1<<3), s"""Funct3 must be shorter than 4 bits. Given: "${funct3.toBinaryString}" """)
    require(funct7 < (1<<7), s"""Funct7 must be shorter than 8 bits. Given: "${funct7.toBinaryString}" """)

    this( itype match {
      case InstructionType.R => s"b${funct7.bitpat(7)} ${"?" * 10} ${funct3.bitpat(3)} ${"?" *  5} ${opcode.bitpat(7)}"
      case InstructionType.I => s"b${"?" * 17} ${funct3.bitpat(3)} ${"?" *  5} ${opcode.bitpat(7)}"
      case InstructionType.S => s"b${"?" * 17} ${funct3.bitpat(3)} ${"?" *  5} ${opcode.bitpat(7)}"
      case InstructionType.B => s"b${"?" * 17} ${funct3.bitpat(3)} ${"?" *  5} ${opcode.bitpat(7)}"
      case InstructionType.U => s"b${"?" * 20} ${"?" *  5} ${opcode.bitpat(7)}"
      case InstructionType.J => s"b${"?" * 20} ${"?" *  5} ${opcode.bitpat(7)}"
      case InstructionType.Z => s"b${"?" * 17} ${funct3.bitpat(3)} ${"?" *  5} ${opcode.bitpat(7)}"
      case _ => assert(false, "Should not reach"); ""
    }, itype, aluOp)
  }

  def apply(bitpat: String, itype: InstructionType, aluOp: ALU_OP): Instruction = this(BitPat(bitpat), itype, aluOp)
  def apply(bitpat: BitPat, itype: InstructionType, aluOp: ALU_OP): Instruction = {
    require(bitpat.getWidth == 32, s"Instruction length must be 32 bits. Provided length: ${bitpat.getWidth}")
    new Instruction(bitpat, itype, aluOp)
  }

  def R(opcode: Int, funct3: Int, funct7: Int, aluOp: ALU_OP) = Instruction(opcode, funct3, funct7, InstructionType.R, aluOp)
  def I(opcode: Int, funct3: Int, aluOp: ALU_OP) = Instruction(opcode, funct3, 0, InstructionType.I, aluOp)
  def S(opcode: Int, funct3: Int, aluOp: ALU_OP) = Instruction(opcode, funct3, 0, InstructionType.S, aluOp)
  def B(opcode: Int, funct3: Int, aluOp: ALU_OP) = Instruction(opcode, funct3, 0, InstructionType.B, aluOp)
  def U(opcode: Int, aluOp: ALU_OP) = Instruction(opcode, 0, 0, InstructionType.U, aluOp)
  def J(opcode: Int, aluOp: ALU_OP) = Instruction(opcode, 0, 0, InstructionType.J, aluOp)
  def Z(opcode: Int, funct3: Int, aluOp: ALU_OP) = Instruction(opcode, funct3, 0, InstructionType.I, aluOp)

}
