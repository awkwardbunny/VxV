package main

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util.BitPat
import dev.meirl.vxv.{Instruction, Instructions, Tile, VxVConfig}

import scala.reflect.runtime.universe._

object Main extends App {
  val path = try args(0) catch { case _: Throwable => "generated_output" }

  val allInstructions = Instructions.getAllInstructionPatterns
  println(s"${allInstructions.size} implemented instructions: ")
  allInstructions foreach(instr => println(s"${instr._1}${" " * (8 - instr._1.length)}: ${instr._2}"))

  val params = new VxVConfig
  val chiselArgs = Array("-E", "verilog", "-td", path)
  (new ChiselStage).execute(chiselArgs, Seq(ChiselGeneratorAnnotation(() => new Test(params))))
}
