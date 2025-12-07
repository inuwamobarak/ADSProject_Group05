// ADS I Class Project
// Chisel Introduction
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 18/10/2022 by Tobias Jauch (@tojauch)

package readserial

import chisel3._
import chisel3.util._


/** controller class */
class Controller extends Module{
  
  val io = IO(new Bundle {
    val rxd   = Input(Bool())      // serial input
    val cntDone = Input(Bool())    // counter reached 8
    val reset = Input(Bool())      // external reset
    val cntEn = Output(Bool())     // enable counter
    val shiftEn = Output(Bool())   // enable shift register
    val valid = Output(Bool())     // data valid signal
  })

  // State definitions
  val sIdle :: sReceiving :: sValid :: Nil = Enum(3)
  val state = RegInit(sIdle)

  // Default outputs
  io.cntEn := false.B
  io.shiftEn := false.B
  io.valid := false.B

  // State machine
  switch(state) {
    is(sIdle) {
      // Wait for start bit (0) when not in reset
      when(!io.reset && !io.rxd) {
        state := sReceiving
        io.cntEn := true.B
        io.shiftEn := true.B
      }
    }
    
    is(sReceiving) {
      io.shiftEn := true.B  // Always shift while receiving
      
      when(io.reset) {
        // Reset during reception
        state := sIdle
      }.elsewhen(io.cntDone) {
        // Finished receiving 8 bits
        state := sValid
        io.valid := true.B
        io.cntEn := false.B
      }.otherwise {
        // Continue receiving
        io.cntEn := true.B
      }
    }
    
    is(sValid) {
      // Valid signal is high for only one cycle
      // State transition happens on next clock
      state := sIdle
    }
  }
}


/** counter class */
class Counter extends Module{
  
  val io = IO(new Bundle {
    val en = Input(Bool())      // count enable
    val reset = Input(Bool())   // reset counter
    val cntDone = Output(Bool()) // counter reached 8
  })

  // 4-bit counter (0-15, but we only need 0-8)
  val count = RegInit(0.U(4.W))

  // Counter logic
  when(io.reset) {
    count := 0.U
  }.elsewhen(io.en && (count < 8.U)) {
    count := count + 1.U
  }.otherwise {
    // Keep value when not enabled or at 8
    count := count
  }

  // Done when count reaches 8 (we've received 8 data bits)
  io.cntDone := (count === 8.U)
  
  // Debug: Reset count when cntDone is true
  when(io.cntDone) {
    count := 0.U
  }
}

/** shift register class */
class ShiftRegister extends Module{
  
  val io = IO(new Bundle {
    val en = Input(Bool())      // shift enable
    val rxd = Input(Bool())     // serial input
    val reset = Input(Bool())   // reset register
    val data = Output(UInt(8.W)) // parallel output
  })

  // 8-bit shift register
  val shiftReg = RegInit(0.U(8.W))

  // Shift register logic
  when(io.reset) {
    shiftReg := 0.U
  }.elsewhen(io.en) {
    // Shift left and insert new bit at LSB
    // MSB is transmitted first, so we shift left
    shiftReg := Cat(shiftReg(6, 0), io.rxd.asUInt)
  }

  io.data := shiftReg
}

/** 
  * The last warm-up task deals with a more complex component. Your goal is to design a serial receiver.
  * It scans an input line (“serial bus”) named rxd for serial transmissions of data bytes. A transmission 
  * begins with a start bit ‘0’ followed by 8 data bits. The most significant bit (MSB) is transmitted first. 
  * There is no parity bit and no stop bit. After the last data bit has been transferred a new transmission 
  * (beginning with a start bit, ‘0’) may immediately follow. If there is no new transmission the bus line 
  * goes high (‘1’, this is considered the “idle” bus signal). In this case the receiver waits until the next 
  * transmission begins. The outputs of the design are an 8-bit parallel data signal and a valid signal. 
  * The valid signal goes high (‘1’) for one clock cycle after the last serial bit has been transmitted, 
  * indicating that a new data byte is ready.
  */
class ReadSerial extends Module{
  
  val io = IO(new Bundle {
    val rxd = Input(Bool())        // serial input
    val reset = Input(Bool())      // external reset
    val data = Output(UInt(8.W))   // parallel data output
    val valid = Output(Bool())     // data valid signal
  })

  // State definitions
  val sIdle :: sReceiving :: sValid :: Nil = Enum(3)
  val state = RegInit(sIdle)
  
  // Counter for received bits (0-7, we receive 8 bits total)
  val bitCount = RegInit(0.U(4.W))
  
  // Shift register for data
  val shiftReg = RegInit(0.U(8.W))
  
  // Output registers
  val dataReg = RegInit(0.U(8.W))
  val validReg = RegInit(false.B)
  
  // Default outputs
  io.data := dataReg
  io.valid := validReg
  
  // Always reset valid after one cycle
  validReg := false.B
  
  // State machine
  switch(state) {
    is(sIdle) {
      // Reset counter and shift register
      bitCount := 0.U
      shiftReg := 0.U
      
      // Wait for start bit (0) on rxd
      when(!io.reset && !io.rxd) {
        state := sReceiving
      }
    }
    
    is(sReceiving) {
      when(io.reset) {
        // Reset during reception
        state := sIdle
      }.otherwise {
        // Sample the current bit into shift register
        // Shift left and insert at LSB (MSB first)
        shiftReg := Cat(shiftReg(6, 0), io.rxd.asUInt)
        
        // Increment bit counter
        bitCount := bitCount + 1.U
        
        // Check if we've received all 8 bits
        when(bitCount === 7.U) {
          // On the 8th bit, store data and go to valid state
          dataReg := Cat(shiftReg(6, 0), io.rxd.asUInt)
          validReg := true.B
          state := sValid
        }
      }
    }
    
    is(sValid) {
      // Valid signal was true for this cycle
      // Now go back to idle to wait for next start bit
      state := sIdle
    }
  }
  
  // Override: If reset is active, go to idle state
  when(io.reset) {
    state := sIdle
    validReg := false.B
    bitCount := 0.U
    shiftReg := 0.U
  }
}