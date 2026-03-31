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

  behavior of "RV32I Pipeline Task 4.2"

  // =========================================
  // BEQ taken
  // =========================================
  it should "execute BEQ taken correctly" in {
  test(new PipelinedRV32I("src/test/programs/BinaryFile_beq_taken"))
    .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

    dut.clock.setTimeout(0)

    dut.clock.step(5)

    dut.io.result.expect(5.U)
    dut.io.exception.expect(false.B)

    dut.clock.step(1)
    dut.io.result.expect(5.U)
    dut.io.exception.expect(false.B)

    dut.clock.step(1)
    dut.io.exception.expect(false.B)

    dut.clock.step(1)
    dut.io.result.expect(1.U)
    dut.io.exception.expect(false.B)

    dut.clock.step(1)
    dut.io.result.expect(9.U)
    dut.io.exception.expect(false.B)
  }
}

  // =========================================
  // BEQ not taken
  // =========================================
  it should "execute BEQ not taken correctly" in {
    test(new PipelinedRV32I("src/test/programs/BinaryFile_beq_not_taken"))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.clock.setTimeout(0)

      // fill pipeline
      dut.clock.step(5)

      // addi x1, x0, 5
      dut.io.result.expect(5.U)
      dut.io.exception.expect(false.B)

      dut.clock.step(1)
      // addi x2, x0, 6
      dut.io.result.expect(6.U)
      dut.io.exception.expect(false.B)

      dut.clock.step(1)
      // beq not taken: no useful writeback
      dut.io.exception.expect(false.B)

      dut.clock.step(1)
      // addi x3, x0, 1 must execute
      dut.io.result.expect(1.U)
      dut.io.exception.expect(false.B)

      dut.clock.step(1)
      // addi x4, x0, 9
      dut.io.result.expect(9.U)
      dut.io.exception.expect(false.B)
    }
  }

  // =========================================
  // BNE taken
  // =========================================
  it should "execute BNE taken correctly" in {
    test(new PipelinedRV32I("src/test/programs/BinaryFile_bne"))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.clock.setTimeout(0)

      // fill pipeline
      dut.clock.step(5)

      // addi x1, x0, 5
      dut.io.result.expect(5.U)
      dut.io.exception.expect(false.B)

      dut.clock.step(1)
      // addi x2, x0, 6
      dut.io.result.expect(6.U)
      dut.io.exception.expect(false.B)

      dut.clock.step(1)
      // bne: no useful writeback
      dut.io.exception.expect(false.B)

      dut.clock.step(1)
      // bubble 1
      dut.io.result.expect(0.U)
      dut.io.exception.expect(false.B)

      dut.clock.step(1)
      // bubble 2
      dut.io.result.expect(0.U)
      dut.io.exception.expect(false.B)

      dut.clock.step(1)
      // target instruction
      dut.io.result.expect(9.U)
      dut.io.exception.expect(false.B)
    }
  }

  // =========================================
  // BLT taken (signed comparison)
  // =========================================
  it should "execute BLT taken correctly" in {
    test(new PipelinedRV32I("src/test/programs/BinaryFile_blt"))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.clock.setTimeout(0)

      // fill pipeline
      dut.clock.step(5)

      // addi x1, x0, -1
      dut.io.result.expect("hffffffff".U)
      dut.io.exception.expect(false.B)

      dut.clock.step(1)
      // addi x2, x0, 1
      dut.io.result.expect(1.U)
      dut.io.exception.expect(false.B)

      dut.clock.step(1)
      // blt: no useful writeback
      dut.io.exception.expect(false.B)

      dut.clock.step(1)
      // bubble 1
      dut.io.result.expect(0.U)
      dut.io.exception.expect(false.B)

      dut.clock.step(1)
      // bubble 2
      dut.io.result.expect(0.U)
      dut.io.exception.expect(false.B)

      dut.clock.step(1)
      // target instruction
      dut.io.result.expect(9.U)
      dut.io.exception.expect(false.B)
    }
  }

  // =========================================
  // JAL
  // =========================================
 it should "execute JAL correctly" in {
  test(new PipelinedRV32I("src/test/programs/BinaryFile_jal"))
    .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

    dut.clock.setTimeout(0)

    dut.clock.step(5)

    dut.io.result.expect(4.U)
    dut.io.exception.expect(false.B)

    dut.clock.step(1)
    dut.io.result.expect(0.U)
    dut.io.exception.expect(false.B)

    dut.clock.step(1)
    dut.io.result.expect(0.U)
    dut.io.exception.expect(false.B)

    dut.clock.step(1)
    dut.io.result.expect(7.U)
    dut.io.exception.expect(false.B)
  }
}

  // =========================================
  // JALR
  // =========================================
  it should "execute JALR correctly" in {
    test(new PipelinedRV32I("src/test/programs/BinaryFile_jalr"))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.clock.setTimeout(0)

      // fill pipeline
      dut.clock.step(5)

      // addi x1, x0, 12
      dut.io.result.expect(12.U)
      dut.io.exception.expect(false.B)

      dut.clock.step(1)
      // jalr at PC=4 -> writes PC+4 = 8
      dut.io.result.expect(8.U)
      dut.io.exception.expect(false.B)

      dut.clock.step(1)
      // bubble 1
      dut.io.result.expect(0.U)
      dut.io.exception.expect(false.B)

      dut.clock.step(1)
      // bubble 2
      dut.io.result.expect(0.U)
      dut.io.exception.expect(false.B)

      dut.clock.step(1)
      // target instruction
      dut.io.result.expect(7.U)
      dut.io.exception.expect(false.B)
    }
  }
}