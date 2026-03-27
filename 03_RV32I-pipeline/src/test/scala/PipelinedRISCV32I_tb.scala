// ADS I Class Project
// Pipelined RISC-V Core
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/15/2023 by Tobias Jauch (@tojauch)

package PipelinedRV32I_Tester

import chisel3._
import chiseltest._
import PipelinedRV32I._
import org.scalatest.flatspec.AnyFlatSpec

class PipelinedRISCV32ITest extends AnyFlatSpec with ChiselScalatestTester {

"RV32I_BasicTester" should "work" in {
    test(new PipelinedRV32I("src/test/programs/BinaryFile_pipelined")).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.clock.setTimeout(0)

      dut.clock.step(5)
      dut.io.result.expect(0.U)     // ADDI x0, x0, 0
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(4.U)     // ADDI x1, x0, 4
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(5.U)     // ADDI x2, x0, 5
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(0.U)     // ADDI x0, x0, 0
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(0.U)     // ADDI x0, x0, 0
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(0.U)     // ADDI x0, x0, 0
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(9.U)     // ADD x3, x1, x2
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(2047.U)  // ADDI x4, x0, 2047
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(16.U)    // ADDI x5, x0, 16
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(0.U)     // ADDI x0, x0, 0
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(0.U)     // ADDI x0, x0, 0
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(0.U)     // ADDI x0, x0, 0
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(2031.U)  // SUB x6, x4, x5
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(0.U)     // ADDI x0, x0, 0
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(0.U)     // ADDI x0, x0, 0
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(0.U)     // ADDI x0, x0, 0
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(2022.U)  // XOR x7, x6, x3
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(2047.U)  // OR x8, x6, x5
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(0.U)     // AND x9, x6, x5
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(0.U)     // ADDI x0, x0, 0
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(64704.U) // SLL x10, x7, x2
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(63.U)    // SRL x11, x7, x2
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(63.U)    // SRA x12, x7, x2
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(0.U)     // SLT x13, x4, x4
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(0.U)     // SLT x13, x4, x5
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(1.U)     // SLT x13, x5, x4
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(0.U)     // SLTU x13, x4, x4
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(0.U)     // SLTU x13, x4, x5
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(1.U)     // SLTU x13, x5, x4
      dut.io.exception.expect(false.B)
      dut.clock.step(1)
      dut.io.result.expect(4095.U)   // ADDI x14, x0, -1 // fff00713
      dut.io.exception.expect(false.B)

      ////////// Task 4.1 Forwarding Unit

      //////////// Resolving the Hazard with NOP ins//////////////////
      //dut.clock.step(1)
      //dut.io.result.expect(0.U)     // first NOP //00000013
      //dut.clock.step(1)
      //dut.io.result.expect(0.U)     // second NOP //00000013
      //dut.clock.step(1)
      //dut.io.result.expect(0.U)     // third NOP  //00000013
      //dut.clock.step(1)
      //dut.io.result.expect(10.U)   // add x2 = x1 + x1 = 5+5=10  //00108133
      ////////////////////////////////////

      // Case 1: EX-to-EX Forwarding
      dut.clock.step(1)
      dut.io.result.expect(5.U)    // addi x1, x0, 5 //      00500093
      dut.clock.step(1)
      dut.io.result.expect(10.U)   // add x2 = x1 + x1 = 5+5=10  //00108133

      // Case 2: Instruction uses x2 with one instruction in between
      dut.clock.step(1)
      dut.io.result.expect(20.U)   // addi x3, x0, 20
      dut.clock.step(1)
      dut.io.result.expect(30.U)   // add x4, x2, x3 -> 10 + 20 = 30 (x2 forwarded from MEM/WB)

      // Css3 3 : If two instructions write to the same register, the most recent one must be forwarded
      dut.clock.step(1)
      dut.io.result.expect(100.U)  // addi x5, x0, 100
      dut.clock.step(1)
      dut.io.result.expect(200.U)  // addi x5, x0, 200
      dut.clock.step(1)
      dut.io.result.expect(400.U)  // add x6, x5, x5 -> Should be 200+200, NOT 100+100



      // ===== Task 4.1 Forwarding tests =====

// Case 1: EX-to-EX forwarding
dut.clock.step(1)
dut.io.result.expect(5.U)      // ADDI x1, x0, 5

dut.clock.step(1)
dut.io.result.expect(10.U)     // ADD x2, x1, x1

// Case 2: dependency with next produced value
dut.clock.step(1)
dut.io.result.expect(20.U)     // ADDI x3, x0, 20

dut.clock.step(1)
dut.io.result.expect(30.U)     // ADD x4, x2, x3

// Case 3: most recent value must be forwarded
dut.clock.step(1)
dut.io.result.expect(100.U)    // ADDI x5, x0, 100

dut.clock.step(1)
dut.io.result.expect(200.U)    // ADDI x5, x0, 200

dut.clock.step(1)
dut.io.result.expect(400.U)    // ADD x6, x5, x5

// Case 4: x0 must remain zero
/*
dut.clock.step(1)
dut.io.result.expect(0.U)      // ADDI x0, x0, 7 ignored
*/
dut.clock.step(1)
dut.io.result.expect(0.U)      // ADD x7, x0, x0


// ===== Task 4.2 Branch tests =====

// ===== Task 4.2 Branch tests =====

// ===== Task 4.2 Branch tests =====

// avancer jusqu’au début
dut.clock.step(4)

// BEQ not taken
dut.io.result.expect(5.U)

dut.clock.step(1)
dut.io.result.expect(6.U)

dut.clock.step(1)
dut.io.result.expect(5.U)

dut.clock.step(1)
dut.io.result.expect(11.U)

// BEQ taken → flush retardé
dut.clock.step(1)
dut.io.result.expect(5.U)

// !!!!!! ici on skip les cycles intermédiaires
dut.clock.step(2)
dut.io.result.expect(22.U)

// JAL → flush retardé
dut.clock.step(2)
dut.io.result.expect(7.U)


    }
  }
}