package dev.meirl.vxv

import chisel3._
import chisel3.util.BitPat

/** Instruction formats */
object InstructionType extends Enumeration {
  type InstructionType = UInt

  /** The R instruction type */
  val R = 0.U(3.W)

  /** The I instruction type */
  val I = 1.U(3.W)

  /** The S instruction type */
  val S = 2.U(3.W)

  /** The B variant of the S instruction type */
  val B = 3.U(3.W)

  /** The U instruction type */
  val U = 4.U(3.W)

  /** The J variant of the U instruction type */
  val J = 5.U(3.W)

  /** The I instruction type, but tells the immediate generator to zero-extend */
  val Z = 6.U(3.W)
}

object WB_MODE extends Enumeration {
  type WB_MODE = UInt
  val WB_ALU = 0.U(2.W)
  val WB_MEM = 1.U(2.W)
  val WB_PC4 = 2.U(2.W)
  val WB_CSR = 3.U(2.W)
}

import dev.meirl.vxv.ALU_OP._
import dev.meirl.vxv.BR_COND._
import dev.meirl.vxv.InstructionType._
import dev.meirl.vxv.ALU_INPUT_A._
import dev.meirl.vxv.ALU_INPUT_B._
import dev.meirl.vxv.WB_MODE._

/** Defines instruction format and behavior
 *
 * @param bitpat [[chisel3.util.BitPat]] to match an instruction
 * @param itype [[InstructionType]] denotes instruction format. Used for immediate generation and register number extraction
 * @param aluOp [[ALU_OP]] to tell ALU what to do
 * @param brCond [[BR_COND]] to determine which branch condition to check
 * */
case class Instruction(bitpat: BitPat, itype: InstructionType, aluOp: ALU_OP, brCond: BR_COND, aluA: ALU_INPUT_A, aluB: ALU_INPUT_B, wbMode: WB_MODE, wbEn: Bool)

object Instructions {
  // Register-Immediate Instructions
  val ADDI  = Instruction.I(0x13, 0x0, ALU_ADD , BR_NONE, ALU_A_REG, ALU_B_IMM, WB_ALU, true.B)
  val SLTI  = Instruction.I(0x13, 0x2, ALU_SLT , BR_NONE, ALU_A_REG, ALU_B_IMM, WB_ALU, true.B)
  val SLTIU = Instruction.I(0x13, 0x3, ALU_SLTU, BR_NONE, ALU_A_REG, ALU_B_IMM, WB_ALU, true.B)
  val XORI  = Instruction.I(0x13, 0x4, ALU_XOR , BR_NONE, ALU_A_REG, ALU_B_IMM, WB_ALU, true.B)
  val ORI   = Instruction.I(0x13, 0x6, ALU_OR  , BR_NONE, ALU_A_REG, ALU_B_IMM, WB_ALU, true.B)
  val ANDI  = Instruction.I(0x13, 0x7, ALU_AND , BR_NONE, ALU_A_REG, ALU_B_IMM, WB_ALU, true.B)

//  val SLLI  = Instruction(0x13, 0x1, 0x00, InstructionType.I)
//  val SRLI  = Instruction(0x13, 0x5, 0x00, InstructionType.I)
//  val SRAI  = Instruction(0x13, 0x5, 0x20, InstructionType.I)
  val SLLI = Instruction("b0000000??????????001?????0010011", InstructionType.I, ALU_SLL, BR_NONE, ALU_A_REG, ALU_B_IMM, WB_ALU, true.B)
  val SRLI = Instruction("b0000000??????????101?????0010011", InstructionType.I, ALU_SRL, BR_NONE, ALU_A_REG, ALU_B_IMM, WB_ALU, true.B)
  val SRAI = Instruction("b0000000??????????101?????0010011", InstructionType.I, ALU_SRA, BR_NONE, ALU_A_REG, ALU_B_IMM, WB_ALU, true.B)

  val LUI   = Instruction.U(0x37, ALU_COPY_B, BR_NONE, ALU_A_PC, ALU_B_IMM, WB_ALU, true.B)
  val AUIPC = Instruction.U(0x17, ALU_ADD   , BR_NONE, ALU_A_PC, ALU_B_IMM, WB_ALU, true.B)

  // Register-Register Instructions
  val ADD  = Instruction.R(0x33, 0x0, 0x00, ALU_ADD , BR_NONE, ALU_A_REG, ALU_B_REG, WB_ALU, true.B)
  val SUB  = Instruction.R(0x33, 0x0, 0x20, ALU_SUB , BR_NONE, ALU_A_REG, ALU_B_REG, WB_ALU, true.B)
  val SLL  = Instruction.R(0x33, 0x1, 0x00, ALU_SLL , BR_NONE, ALU_A_REG, ALU_B_REG, WB_ALU, true.B)
  val SLT  = Instruction.R(0x33, 0x2, 0x00, ALU_SLT , BR_NONE, ALU_A_REG, ALU_B_REG, WB_ALU, true.B)
  val SLTU = Instruction.R(0x33, 0x3, 0x00, ALU_SLTU, BR_NONE, ALU_A_REG, ALU_B_REG, WB_ALU, true.B)
  val XOR  = Instruction.R(0x33, 0x4, 0x00, ALU_XOR , BR_NONE, ALU_A_REG, ALU_B_REG, WB_ALU, true.B)
  val SRL  = Instruction.R(0x33, 0x5, 0x00, ALU_SRL , BR_NONE, ALU_A_REG, ALU_B_REG, WB_ALU, true.B)
  val SRA  = Instruction.R(0x33, 0x5, 0x20, ALU_SRA , BR_NONE, ALU_A_REG, ALU_B_REG, WB_ALU, true.B)
  val OR   = Instruction.R(0x33, 0x6, 0x00, ALU_OR  , BR_NONE, ALU_A_REG, ALU_B_REG, WB_ALU, true.B)
  val AND  = Instruction.R(0x33, 0x7, 0x00, ALU_AND , BR_NONE, ALU_A_REG, ALU_B_REG, WB_ALU, true.B)

  // Unconditional Jumps
  val JAL   = Instruction.J(0x6F, ALU_ADD, BR_NONE, ALU_A_PC, ALU_B_IMM, WB_PC4, true.B)
  val JALR  = Instruction.I(0x67, 0x0, ALU_ADD, BR_NONE, ALU_A_REG, ALU_B_IMM, WB_PC4, true.B)

  // Conditional Branches
  val BEQ  = Instruction.B(0x63, 0x0, ALU_ADD, BR_EQ , ALU_A_PC, ALU_B_IMM, WB_ALU, false.B)
  val BNE  = Instruction.B(0x63, 0x1, ALU_ADD, BR_NE , ALU_A_PC, ALU_B_IMM, WB_ALU, false.B)
  val BLT  = Instruction.B(0x63, 0x4, ALU_ADD, BR_LT , ALU_A_PC, ALU_B_IMM, WB_ALU, false.B)
  val BGE  = Instruction.B(0x63, 0x5, ALU_ADD, BR_GE , ALU_A_PC, ALU_B_IMM, WB_ALU, false.B)
  val BLTU = Instruction.B(0x63, 0x6, ALU_ADD, BR_LTU, ALU_A_PC, ALU_B_IMM, WB_ALU, false.B)
  val BGEU = Instruction.B(0x63, 0x7, ALU_ADD, BR_GEU, ALU_A_PC, ALU_B_IMM, WB_ALU, false.B)

  // Load and Store Instructions
  val LB  = Instruction.I(0x03, 0x0, ALU_ADD, BR_NONE, ALU_A_REG, ALU_B_IMM, WB_MEM, true.B)
  val LH  = Instruction.I(0x03, 0x1, ALU_ADD, BR_NONE, ALU_A_REG, ALU_B_IMM, WB_MEM, true.B)
  val LW  = Instruction.I(0x03, 0x2, ALU_ADD, BR_NONE, ALU_A_REG, ALU_B_IMM, WB_MEM, true.B)
  val LBU = Instruction.I(0x03, 0x4, ALU_ADD, BR_NONE, ALU_A_REG, ALU_B_IMM, WB_MEM, true.B)
  val LHU = Instruction.I(0x03, 0x5, ALU_ADD, BR_NONE, ALU_A_REG, ALU_B_IMM, WB_MEM, false.B)
  val SB  = Instruction.S(0x23, 0x0, ALU_ADD, BR_NONE, ALU_A_REG, ALU_B_IMM, WB_MEM, false.B)
  val SH  = Instruction.S(0x23, 0x1, ALU_ADD, BR_NONE, ALU_A_REG, ALU_B_IMM, WB_MEM, false.B)
  val SW  = Instruction.S(0x23, 0x2, ALU_ADD, BR_NONE, ALU_A_REG, ALU_B_IMM, WB_MEM, false.B)

  // Fence
//  val FENCE = Instruction.I(0x0F, 0x0, ALU_NOP)
//  val FENCE_I = Instruction.I(0x0F, 0x1, ALU_NOP)

  // CSR
  val CSRRW   = Instruction.I(0x73, 0x1, ALU_COPY_A, BR_NONE, ALU_A_REG, ALU_B_NONE, WB_CSR, true.B)
  val CSRRS   = Instruction.I(0x73, 0x2, ALU_COPY_A, BR_NONE, ALU_A_REG, ALU_B_NONE, WB_CSR, true.B)
  val CSRRC   = Instruction.I(0x73, 0x3, ALU_COPY_A, BR_NONE, ALU_A_REG, ALU_B_NONE, WB_CSR, true.B)
  val CSRRWI  = Instruction.Z(0x73, 0x5, ALU_NOP   , BR_NONE, ALU_A_NONE, ALU_B_NONE, WB_CSR, true.B)
  val CSRRSI  = Instruction.Z(0x73, 0x6, ALU_NOP   , BR_NONE, ALU_A_NONE, ALU_B_NONE, WB_CSR, true.B)
  val CSRRCI  = Instruction.Z(0x73, 0x7, ALU_NOP   , BR_NONE, ALU_A_NONE, ALU_B_NONE, WB_CSR, true.B)

  // Environment Call and Breakpoints
  val ECALL  = Instruction("b00000000000000000000000001110011", InstructionType.I, ALU_NOP, BR_NONE, ALU_A_NONE, ALU_B_NONE, WB_CSR, false.B)
  val EBREAK = Instruction("b00000000000100000000000001110011", InstructionType.I, ALU_NOP, BR_NONE, ALU_A_NONE, ALU_B_NONE, WB_CSR, false.B)

  // NOP Instruction for filling in registers during initialization and stall
  val NOP = BitPat.bitPatToUInt(BitPat("b00000000000000000000000000010011"))

  /** Debugging helper method to fetch all instruction patterns using reflection
   *
   * @return List of (name, bitPattern) tuples of strings
   * */
  def getAllInstructionPatterns() = {
    import scala.reflect.runtime.universe._
    val rm = scala.reflect.runtime.currentMirror
    val instanceMirror = rm.reflect(Instructions)
    val instrs = rm.classSymbol(Instructions.getClass).toType.members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic && instanceMirror.reflectMethod(m)().isInstanceOf[Instruction] => (m.name.toString, instanceMirror.reflectMethod(m)().asInstanceOf[Instruction])
    }

    instrs.map(acc => (acc._1 -> bitpatToString(acc._2.bitpat))).toList.reverse
  }

  def toMap() = {
    import scala.reflect.runtime.universe._
    val rm = scala.reflect.runtime.currentMirror
    val instanceMirror = rm.reflect(Instructions)
    rm.classSymbol(Instructions.getClass).toType.members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic && instanceMirror.reflectMethod(m)().isInstanceOf[Instruction] => {
        val inst = instanceMirror.reflectMethod(m)().asInstanceOf[Instruction]
        (inst.bitpat, inst)
      }
    }.toMap
  }

  def toSeq() = {
    import scala.reflect.runtime.universe._
    val rm = scala.reflect.runtime.currentMirror
    val instanceMirror = rm.reflect(Instructions)
    rm.classSymbol(Instructions.getClass).toType.members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic && instanceMirror.reflectMethod(m)().isInstanceOf[Instruction] => {
        val inst = instanceMirror.reflectMethod(m)().asInstanceOf[Instruction]
        (inst.bitpat, inst)
      }
    }.toSeq
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

  /** Helper function to create instruction from its fields */
  def apply(opcode: Int, funct3: Int, funct7: Int, itype: InstructionType, aluOp: ALU_OP, brCond: BR_COND, aluA: ALU_INPUT_A, aluB: ALU_INPUT_B, wbMode: WB_MODE, wbEn: Bool): Instruction = {
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
    }, itype, aluOp, brCond, aluA, aluB, wbMode, wbEn)
  }

  /** Helper function to create instruction from [[scala.Predef.String]] */
  def apply(bitpat: String, itype: InstructionType, aluOp: ALU_OP, brCond: BR_COND, aluA: ALU_INPUT_A, aluB: ALU_INPUT_B, wbMode: WB_MODE, wbEn: Bool): Instruction = this(BitPat(bitpat), itype, aluOp, brCond, aluA, aluB, wbMode, wbEn)

  /** Helper function to create instruction from [[chisel3.util.BitPat]] */
  def apply(bitpat: BitPat, itype: InstructionType, aluOp: ALU_OP, brCond: BR_COND, aluA: ALU_INPUT_A, aluB: ALU_INPUT_B, wbMode: WB_MODE, wbEn: Bool): Instruction = {
    require(bitpat.getWidth == 32, s"Instruction length must be 32 bits. Provided length: ${bitpat.getWidth}")
    new Instruction(bitpat, itype, aluOp, brCond, aluA, aluB, wbMode, wbEn)
  }

  /** Helper function to create R-type instruction */
  def R(opcode: Int, funct3: Int, funct7: Int, aluOp: ALU_OP, brCond: BR_COND, aluA: ALU_INPUT_A, aluB: ALU_INPUT_B, wbMode: WB_MODE, wbEn: Bool) = Instruction(opcode, funct3, funct7, InstructionType.R, aluOp, brCond, aluA, aluB, wbMode, wbEn)

  /** Helper function to create I-type instruction */
  def I(opcode: Int, funct3: Int, aluOp: ALU_OP, brCond: BR_COND, aluA: ALU_INPUT_A, aluB: ALU_INPUT_B, wbMode: WB_MODE, wbEn: Bool) = Instruction(opcode, funct3, 0, InstructionType.I, aluOp, brCond, aluA, aluB, wbMode, wbEn)

  /** Helper function to create S-type instruction */
  def S(opcode: Int, funct3: Int, aluOp: ALU_OP, brCond: BR_COND, aluA: ALU_INPUT_A, aluB: ALU_INPUT_B, wbMode: WB_MODE, wbEn: Bool) = Instruction(opcode, funct3, 0, InstructionType.S, aluOp, brCond, aluA, aluB, wbMode, wbEn)

  /** Helper function to create B-type instruction */
  def B(opcode: Int, funct3: Int, aluOp: ALU_OP, brCond: BR_COND, aluA: ALU_INPUT_A, aluB: ALU_INPUT_B, wbMode: WB_MODE, wbEn: Bool) = Instruction(opcode, funct3, 0, InstructionType.B, aluOp, brCond, aluA, aluB, wbMode, wbEn)

  /** Helper function to create U-type instruction */
  def U(opcode: Int, aluOp: ALU_OP, brCond: BR_COND, aluA: ALU_INPUT_A, aluB: ALU_INPUT_B, wbMode: WB_MODE, wbEn: Bool) = Instruction(opcode, 0, 0, InstructionType.U, aluOp, brCond, aluA, aluB, wbMode, wbEn)

  /** Helper function to create J-type instruction */
  def J(opcode: Int, aluOp: ALU_OP, brCond: BR_COND, aluA: ALU_INPUT_A, aluB: ALU_INPUT_B, wbMode: WB_MODE, wbEn: Bool) = Instruction(opcode, 0, 0, InstructionType.J, aluOp, brCond, aluA, aluB, wbMode, wbEn)

  /** Helper function to create Z-type instruction */
  def Z(opcode: Int, funct3: Int, aluOp: ALU_OP, brCond: BR_COND, aluA: ALU_INPUT_A, aluB: ALU_INPUT_B, wbMode: WB_MODE, wbEn: Bool) = Instruction(opcode, funct3, 0, InstructionType.I, aluOp, brCond, aluA, aluB, wbMode, wbEn)

}
