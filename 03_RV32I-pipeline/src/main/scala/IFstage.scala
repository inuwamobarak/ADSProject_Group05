// ADS I Class Project
// Pipelined RISC-V Core - IF Stage
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)

/*
The Instruction Fetch (IF) stage is the first stage of the pipeline and handles instruction retrieval from memory.

Memory:
    IMem: instruction memory with 4096 32-bit unsigned integer entires, loaded from a binary file at compile time

Internal Registers:
    PC: 32-bit unsigned integer register, initialized to 0 holding the current program counter address

Internal Signals:
    none

Functionality:
    Fetch the instruction at the current PC (word-aligned addressing)
    Increment the PC (word-aligned) each clock cycle to fetch the next sequential instruction

Parameters:
    BinaryFile: String - path to the binary file to load into instruction memory

Inputs:
    none

Outputs:
    instr: send the fetched instruction to IF Barrier
*/

package core_tile

import chisel3._
import chisel3.util.experimental.loadMemoryFromFile

// -----------------------------------------
// Fetch Stage
// -----------------------------------------

class IF (BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    val instr = Output(UInt(32.W))
    val pc      = Output(UInt(32.W))
 
    // Control hazard redirect (from EX or ID stage)
    val pcRedirect    = Input(Bool())
    val redirectTarget = Input(UInt(32.W))
 
    // BTB prediction inputs (from BTB, consulted every cycle)
    val btbValid       = Input(Bool())
    val btbTarget      = Input(UInt(32.W))
    val btbPredTaken   = Input(Bool())
    val useBTB         = Input(Bool())   // enable/disable BTB (switch static vs dynamic)
  })

  val pc = RegInit(0.U(32.W))

  val imem = Mem(4096, UInt(32.W))
  loadMemoryFromFile(imem, BinaryFile)
  io.instr := imem(pc >> 2)
  io.pc    := pc
 
  // PC update logic
  when(io.pcRedirect) {
    // Flush/redirect always wins (misprediction or unconditional jump resolved in ID/EX)
    pc := io.redirectTarget
  }.elsewhen(io.useBTB && io.btbValid && io.btbPredTaken) {
    // BTB predicts taken: jump to predicted target
    pc := io.btbTarget
  }.otherwise {
    pc := pc + 4.U
  }
}