// ADS I Class Project
// Chisel Introduction
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 18/10/2022 by Tobias Jauch (@tojauch)

package readserial

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


/** 
  *read serial tester
  */
class ReadSerialTester extends AnyFlatSpec with ChiselScalatestTester {

  "ReadSerial" should "work" in {
    test(new ReadSerial).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      // Start with idle state
      dut.io.rxd.poke(true.B)   // idle line high
      dut.io.reset.poke(false.B)
      dut.clock.step(5)         // Wait in idle
      dut.io.valid.expect(false.B)
      
      // Send start bit (0)
      dut.io.rxd.poke(false.B)  // start bit
      dut.clock.step(1)
      
      // Send data byte 0x55 (01010101) - MSB first
      // So we send: 0,1,0,1,0,1,0,1
      val data = 0x55
      for (i <- 7 to 0 by -1) {
        val bit = (data >> i) & 1
        // Convert bit to Bool: 1 -> true.B, 0 -> false.B
        if (bit == 1) {
          dut.io.rxd.poke(true.B)
        } else {
          dut.io.rxd.poke(false.B)
        }
        dut.clock.step(1)
      }
      
      // Check valid signal and data
      dut.io.valid.expect(true.B)
      dut.io.data.expect(data.U)
      dut.clock.step(1)
      dut.io.valid.expect(false.B)  // valid for only one cycle
      
      // Should go back to idle
      dut.io.rxd.poke(true.B)
      dut.clock.step(5)
      dut.io.valid.expect(false.B)
    }
  }
  
  it should "receive multiple bytes in sequence" in {
    test(new ReadSerial).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      dut.io.rxd.poke(true.B)
      dut.io.reset.poke(false.B)
      dut.clock.step(3)
      
      // First byte: 0xAA (10101010)
      dut.io.rxd.poke(false.B)  // start bit
      dut.clock.step(1)
      
      val data1 = 0xAA
      for (i <- 7 to 0 by -1) {
        val bit = (data1 >> i) & 1
        if (bit == 1) {
          dut.io.rxd.poke(true.B)
        } else {
          dut.io.rxd.poke(false.B)
        }
        dut.clock.step(1)
      }
      
      dut.io.valid.expect(true.B)
      dut.io.data.expect(data1.U)
      dut.clock.step(1)
      dut.io.valid.expect(false.B)
      
      // Second byte immediately: 0x55 (01010101)
      dut.io.rxd.poke(false.B)  // start bit of next byte
      dut.clock.step(1)
      
      val data2 = 0x55
      for (i <- 7 to 0 by -1) {
        val bit = (data2 >> i) & 1
        if (bit == 1) {
          dut.io.rxd.poke(true.B)
        } else {
          dut.io.rxd.poke(false.B)
        }
        dut.clock.step(1)
      }
      
      dut.io.valid.expect(true.B)
      dut.io.data.expect(data2.U)
      dut.clock.step(1)
      dut.io.valid.expect(false.B)
    }
  }
  
  it should "reset correctly during transmission" in {
    test(new ReadSerial).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      // Start receiving
      dut.io.rxd.poke(false.B)  // start bit
      dut.io.reset.poke(false.B)
      dut.clock.step(1)
      
      // Send a few bits
      dut.io.rxd.poke(true.B)
      dut.clock.step(1)
      dut.io.rxd.poke(false.B)
      dut.clock.step(1)
      dut.io.rxd.poke(true.B)
      dut.clock.step(1)
      
      // Reset during reception
      dut.io.reset.poke(true.B)
      dut.clock.step(1)
      dut.io.reset.poke(false.B)
      
      // Should be back in idle, no valid signal
      dut.io.rxd.poke(true.B)
      dut.clock.step(3)
      dut.io.valid.expect(false.B)
      
      // Should be able to start new transmission after reset
      dut.io.rxd.poke(false.B)  // new start bit
      dut.clock.step(1)
      
      val data = 0xFF
      for (i <- 7 to 0 by -1) {
        dut.io.rxd.poke(true.B)  // all 1s
        dut.clock.step(1)
      }
      
      dut.io.valid.expect(true.B)
      dut.io.data.expect(data.U)
    }
  }
  
  it should "ignore idle line correctly" in {
    test(new ReadSerial).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      // Idle for some time
      dut.io.rxd.poke(true.B)
      dut.io.reset.poke(false.B)
      dut.clock.step(10)
      dut.io.valid.expect(false.B)
      
      // Then start transmission
      dut.io.rxd.poke(false.B)  // start bit
      dut.clock.step(1)
      
      val data = 0x81  // 10000001
      for (i <- 7 to 0 by -1) {
        val bit = (data >> i) & 1
        if (bit == 1) {
          dut.io.rxd.poke(true.B)
        } else {
          dut.io.rxd.poke(false.B)
        }
        dut.clock.step(1)
      }
      
      dut.io.valid.expect(true.B)
      dut.io.data.expect(data.U)
    }
  } 
}