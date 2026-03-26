package core_tile

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

// -----------------------------------------
// 2-Way Set-Associative Branch Target Buffer
// 8 sets, 2-bit saturating predictor, LRU replacement
// -----------------------------------------

object PredState extends ChiselEnum {
  val strongNotTaken = Value(0.U)
  val weakNotTaken   = Value(1.U)
  val weakTaken      = Value(2.U)
  val strongTaken    = Value(3.U)
}

class BTBEntry extends Bundle {
  val valid  = Bool()
  val tag    = UInt(25.W)   // PC[31:7] — upper bits after removing index[4:2] and offset[1:0]
  val target = UInt(32.W)
  val pred   = UInt(2.W)    // 2-bit predictor state (PredState encoding)
}

// Number of sets = 8 → index bits = 3 → PC[4:2]
// Tag = PC[31:5] = 27 bits, but since PC[1:0] always 0, effectively PC[31:2] without index
// We use PC[31:5] as tag (27 bits) for 8-set BTB
// Actually: PC[4:2] = index (3 bits), PC[1:0] = 0 (ignored), tag = PC[31:5] (27 bits)

class BTB extends Module {
  val io = IO(new Bundle {
    // Lookup (from IF stage, current PC)
    val PC          = Input(UInt(32.W))
    val isBranch     = Input(Bool())       // used externally; BTB only acts when update=true
    val valid       = Output(Bool())
    val target      = Output(UInt(32.W))
    val predictTaken = Output(Bool())

    // Update (from EX stage, 2 cycles after fetch)
    val update        = Input(Bool())
    val updatePC      = Input(UInt(32.W))
    val updateTarget  = Input(UInt(32.W))
    val mispredicted  = Input(Bool())
  })

  val NUM_SETS = 8
  val NUM_WAYS = 2
  val TAG_BITS = 27   // PC[31:5]

  // BTB storage: 8 sets × 2 ways
  // Using Reg of Vec for ways
  val btbValid  = RegInit(VecInit(Seq.fill(NUM_SETS)(VecInit(Seq.fill(NUM_WAYS)(false.B)))))
  val btbTag    = Reg(Vec(NUM_SETS, Vec(NUM_WAYS, UInt(TAG_BITS.W))))
  val btbTarget = Reg(Vec(NUM_SETS, Vec(NUM_WAYS, UInt(32.W))))
  val btbPred   = RegInit(VecInit(Seq.fill(NUM_SETS)(VecInit(Seq.fill(NUM_WAYS)(PredState.weakTaken.asUInt)))))
  // LRU bit per set: 0 = way 0 is LRU, 1 = way 1 is LRU
  val lru       = RegInit(VecInit(Seq.fill(NUM_SETS)(false.B)))

  // Helper: extract index and tag from a PC
  def getIndex(pc: UInt): UInt = pc(4, 2)
  def getTag(pc: UInt):   UInt = pc(31, 5)

  // -------------------------
  // Lookup logic
  // -------------------------
  val lookupIndex = getIndex(io.PC)
  val lookupTag   = getTag(io.PC)

  val way0Valid = btbValid(lookupIndex)(0)
  val way1Valid = btbValid(lookupIndex)(1)
  val way0Hit   = way0Valid && (btbTag(lookupIndex)(0) === lookupTag)
  val way1Hit   = way1Valid && (btbTag(lookupIndex)(1) === lookupTag)
  val anyHit    = way0Hit || way1Hit
  val hitWay    = Mux(way0Hit, 0.U, 1.U)
  val hitPred   = Mux(way0Hit, btbPred(lookupIndex)(0), btbPred(lookupIndex)(1))
  val hitTarget = Mux(way0Hit, btbTarget(lookupIndex)(0), btbTarget(lookupIndex)(1))

  io.valid        := anyHit
  io.target       := hitTarget
  io.predictTaken := hitPred >= PredState.weakTaken.asUInt  // taken if state >= 2

  // Update LRU on hit (mark hit way as recently used → other way becomes LRU)
  when(anyHit) {
    lru(lookupIndex) := !hitWay.asBool
  }

  // -------------------------
  // Update logic
  // -------------------------
  val updateIndex = getIndex(io.updatePC)
  val updateTag   = getTag(io.updatePC)

  // Find if the updating PC is already in the BTB
  val upd0Hit = btbValid(updateIndex)(0) && (btbTag(updateIndex)(0) === updateTag)
  val upd1Hit = btbValid(updateIndex)(1) && (btbTag(updateIndex)(1) === updateTag)
  val updHit  = upd0Hit || upd1Hit
  val updWay  = Mux(upd0Hit, 0.U, 1.U)

  // LRU way for replacement
  val lruWay = lru(updateIndex).asUInt  // 0 or 1

  when(io.update) {
    when(updHit) {
      // Entry exists: update target and predictor state
      val w = updWay
      btbTarget(updateIndex)(w) := io.updateTarget

      // Update 2-bit predictor FSM
      val curPred = btbPred(updateIndex)(w)
      val newPred = WireDefault(curPred)
      when(io.mispredicted) {
        // Branch was predicted taken but wasn't, or vice versa — move toward actual
        // mispredicted = actual outcome != prediction
        // If currently >= weakTaken (predicted taken) but actually not taken → decrement
        // If currently < weakTaken (predicted not taken) but actually taken → increment
        when(curPred >= PredState.weakTaken.asUInt) {
          // was predicting taken, actual = not taken
          newPred := curPred - 1.U
        }.otherwise {
          // was predicting not taken, actual = taken
          newPred := curPred + 1.U
        }
      }.otherwise {
        // Correct prediction: move toward extremes (strengthen)
        when(curPred >= PredState.weakTaken.asUInt) {
          // predicted taken and was taken
          when(curPred < PredState.strongTaken.asUInt) { newPred := curPred + 1.U }
        }.otherwise {
          // predicted not taken and was not taken
          when(curPred > PredState.strongNotTaken.asUInt) { newPred := curPred - 1.U }
        }
      }
      btbPred(updateIndex)(w) := newPred
      // Update LRU: this way was just used
      lru(updateIndex) := !w.asBool

    }.otherwise {
      // New entry: install at LRU way
      val w = lruWay
      btbValid(updateIndex)(w)  := true.B
      btbTag(updateIndex)(w)    := updateTag
      btbTarget(updateIndex)(w) := io.updateTarget
      // Initialize predictor to weakTaken (good for loops)
      btbPred(updateIndex)(w)   := PredState.weakTaken.asUInt
      // New entry = recently used, other way becomes LRU
      lru(updateIndex) := !w.asBool
    }
  }
}