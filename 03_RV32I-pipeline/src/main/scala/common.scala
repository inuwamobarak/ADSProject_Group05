// ADS I Class Project
// Pipelined RISC-V Core - Common Definitions
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)

/*
Global Definitions and Data Types

Enumerations:
    uopc: ChiselEnum defining micro-operation codes for all supported RV32I instructions:
        R-type instructions 
        I-type instructions
        NOP (no operation, default case)

This enum is used throughout the pipeline:
    Decode stage assigns uop based on instruction fields
    Execute stage maps uop to ALU operations
*/

package core_tile

import chisel3._
import chisel3.experimental.ChiselEnum

// -----------------------------------------
// Global Definitions and Data Types
// -----------------------------------------

//ToDo: Add your implementation according to the specification above here 
object uopc extends ChiselEnum {
  val NOP = Value

  // R-type
  val ADD, SUB, XOR, OR, AND = Value
  val SLL, SRL, SRA = Value
  val SLT, SLTU = Value

  // I-type
  val ADDI, XORI, ORI, ANDI = Value
  val SLLI, SRLI, SRAI = Value
  val SLTI, SLTIU = Value
}