// ADS I Class Project
// Pipelined RISC-V Core - IF Barrier
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)

/*
IF-Barrier: pipeline register between Fetch and Decode stages

Internal Registers:
    instrReg: holds instruction between pipeline stages, initialized to 0

Inputs:
    inInstr: fetched instruction from IF stage

Outputs:
    outInstr: instruction to ID stage

Functionality:
    Save all input signals to a register and output them in the following clock cycle
*/

package core_tile

import chisel3._

// -----------------------------------------
// IF-Barrier
// -----------------------------------------

class IFBarrier extends Module {
  val io = IO(new Bundle {
    val inInstr  = Input(UInt(32.W))
    val outInstr = Output(UInt(32.W))
 
    val inPC  = Input(UInt(32.W))
    val outPC = Output(UInt(32.W))
 
    // Capture BTB prediction at fetch time so it travels with the instruction
    val inBTBPredTaken  = Input(Bool())
    val outBTBPredTaken = Output(Bool())
 
    // Flush: insert NOP (used when a branch/jump redirects PC)
    val flush = Input(Bool())
  })

  // NOP = addi x0, x0, 0 = 0x00000013
  val NOP = "h00000013".U(32.W)
 
  val instrReg = RegInit(NOP)
  val pcReg    = RegInit(0.U(32.W))
 
  instrReg := Mux(io.flush, NOP, io.inInstr)
  pcReg    := io.inPC
 
  io.outInstr        := instrReg
  io.outPC           := pcReg
  io.outBTBPredTaken := RegNext(Mux(io.flush, false.B, io.inBTBPredTaken), false.B)
}