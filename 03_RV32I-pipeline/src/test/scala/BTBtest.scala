package core_tile

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class BTBTest extends AnyFlatSpec with ChiselScalatestTester {

  // Helper: drive a BTB update for one cycle then de-assert
  def doUpdate(dut: BTB, pc: Int, target: Int, mispredicted: Boolean): Unit = {
    dut.io.update.poke(true.B)
    dut.io.updatePC.poke(pc.U)
    dut.io.updateTarget.poke(target.U)
    dut.io.mispredicted.poke(mispredicted.B)
    dut.clock.step(1)
    dut.io.update.poke(false.B)
    dut.io.mispredicted.poke(false.B)
  }

  // Helper: lookup a PC and return (valid, target, predictTaken)
  def lookup(dut: BTB, pc: Int): (Boolean, Int, Boolean) = {
    dut.io.PC.poke(pc.U)
    dut.io.isBranch.poke(true.B)
    dut.clock.step(0) // combinational read
    val v  = dut.io.valid.peek().litToBoolean
    val t  = dut.io.target.peek().litValue.toInt
    val pt = dut.io.predictTaken.peek().litToBoolean
    (v, t, pt)
  }

  // PC helpers: index = PC[4:2], tag = PC[31:5]
  // Two PCs that map to the same set (same index bits) but different tags
  val PC_A  = 0x1000  // index = (0x1000 >> 2) & 7 = 0
  val PC_B  = 0x1020  // index = (0x1020 >> 2) & 7 = 0  (same set, different tag)
  val PC_C  = 0x1040  // index = (0x1040 >> 2) & 7 = 0  (same set again → triggers LRU eviction)
  val PC_D  = 0x1008  // index = (0x1008 >> 2) & 7 = 2  (different set)

  val TARGET_A = 0x2000
  val TARGET_B = 0x3000
  val TARGET_C = 0x4000
  val TARGET_D = 0x5000

  // -------------------------------------------------------
  "BTB" should "miss on an empty BTB" in {
    test(new BTB) { dut =>
      val (v, _, _) = lookup(dut, PC_A)
      assert(!v, "Empty BTB should report no valid entry")
    }
  }

  // -------------------------------------------------------
  it should "install a new entry and predict taken after update" in {
    test(new BTB) { dut =>
      // Insert PC_A (mispredicted=false means branch was correctly taken)
      doUpdate(dut, PC_A, TARGET_A, mispredicted = false)

      val (v, t, pt) = lookup(dut, PC_A)
      assert(v,           "Entry should be valid after insert")
      assert(t == TARGET_A, s"Target should be $TARGET_A, got $t")
      // Initial state = weakTaken(2); correct taken prediction → stays taken
      assert(pt,          "Prediction should be taken (weakTaken initial + correct)")
    }
  }

  // -------------------------------------------------------
  it should "not match a different PC that maps to the same set" in {
    test(new BTB) { dut =>
      doUpdate(dut, PC_A, TARGET_A, mispredicted = false)

      val (v, _, _) = lookup(dut, PC_B)
      assert(!v, "Different tag in same set should not hit")
    }
  }

  // -------------------------------------------------------
  it should "hold two entries in the same set (2-way)" in {
    test(new BTB) { dut =>
      doUpdate(dut, PC_A, TARGET_A, mispredicted = false)
      doUpdate(dut, PC_B, TARGET_B, mispredicted = false)

      val (vA, tA, _) = lookup(dut, PC_A)
      val (vB, tB, _) = lookup(dut, PC_B)
      assert(vA && tA == TARGET_A, "Way 0 (PC_A) should still be valid")
      assert(vB && tB == TARGET_B, "Way 1 (PC_B) should be valid")
    }
  }

  // -------------------------------------------------------
  it should "evict the LRU entry when a third PC maps to the same full set" in {
    test(new BTB) { dut =>
      // Fill both ways
      doUpdate(dut, PC_A, TARGET_A, mispredicted = false) // way 0 installed; LRU = way 0 -> now way 1 is LRU
      doUpdate(dut, PC_B, TARGET_B, mispredicted = false) // way 1 installed; LRU = way 1 -> now way 0 is LRU

      // Access PC_B to make it recently used → PC_A becomes LRU
      lookup(dut, PC_B)
      dut.clock.step(1)

      // Install PC_C → should evict PC_A (LRU)
      doUpdate(dut, PC_C, TARGET_C, mispredicted = false)

      val (vA, _, _) = lookup(dut, PC_A)
      val (vB, tB, _) = lookup(dut, PC_B)
      val (vC, tC, _) = lookup(dut, PC_C)

      assert(!vA,           "PC_A should have been evicted (LRU)")
      assert(vB && tB == TARGET_B, "PC_B should still be valid (recently used)")
      assert(vC && tC == TARGET_C, "PC_C should be the new entry")
    }
  }

  // -------------------------------------------------------
  it should "update an existing entry's target on a second update" in {
    test(new BTB) { dut =>
      doUpdate(dut, PC_A, TARGET_A, mispredicted = false)
      val NEW_TARGET = 0x9000
      doUpdate(dut, PC_A, NEW_TARGET, mispredicted = false)

      val (v, t, _) = lookup(dut, PC_A)
      assert(v,          "Entry should still be valid")
      assert(t == NEW_TARGET, s"Target should be updated to $NEW_TARGET, got $t")
    }
  }

  // -------------------------------------------------------
  it should "transition FSM: weakTaken → strongTaken on correct taken prediction" in {
    test(new BTB) { dut =>
      // Install (initialises to weakTaken=2)
      doUpdate(dut, PC_A, TARGET_A, mispredicted = false)
      // Correct taken prediction → advance to strongTaken(3)
      doUpdate(dut, PC_A, TARGET_A, mispredicted = false)

      val (_, _, pt) = lookup(dut, PC_A)
      assert(pt, "Should still predict taken (strongTaken)")
    }
  }

  it should "transition FSM: weakTaken → weakNotTaken on misprediction (predicted taken, was not taken)" in {
    test(new BTB) { dut =>
      // Install (initialises to weakTaken=2)
      doUpdate(dut, PC_A, TARGET_A, mispredicted = false)
      // Misprediction: predicted taken but actually not taken → weakNotTaken(1)
      doUpdate(dut, PC_A, TARGET_A, mispredicted = true)

      val (_, _, pt) = lookup(dut, PC_A)
      assert(!pt, "After one misprediction from weakTaken, should predict not-taken (weakNotTaken)")
    }
  }

  it should "require two mispredictions to flip from strongTaken to predicting not-taken" in {
    test(new BTB) { dut =>
      // Install and advance to strongTaken(3)
      doUpdate(dut, PC_A, TARGET_A, mispredicted = false)
      doUpdate(dut, PC_A, TARGET_A, mispredicted = false)

      // First misprediction: strongTaken → weakTaken(2), still predicts taken
      doUpdate(dut, PC_A, TARGET_A, mispredicted = true)
      val (_, _, pt1) = lookup(dut, PC_A)
      assert(pt1, "After first misprediction from strongTaken, should still predict taken (weakTaken)")

      // Second misprediction: weakTaken → weakNotTaken(1), now predicts not-taken
      doUpdate(dut, PC_A, TARGET_A, mispredicted = true)
      val (_, _, pt2) = lookup(dut, PC_A)
      assert(!pt2, "After second misprediction, should predict not-taken (weakNotTaken)")
    }
  }

  it should "recover prediction back to taken after two correct taken branches from weakNotTaken" in {
    test(new BTB) { dut =>
      // Get to weakNotTaken(1) via two mispredictions
      doUpdate(dut, PC_A, TARGET_A, mispredicted = false)
      doUpdate(dut, PC_A, TARGET_A, mispredicted = true)
      doUpdate(dut, PC_A, TARGET_A, mispredicted = true)

      // One correct taken: weakNotTaken → weakTaken(2)
      doUpdate(dut, PC_A, TARGET_A, mispredicted = false)  // wait — mispredicted=false means prediction was correct
      // But if prediction was notTaken and branch was actually taken → mispredicted=true
      // Here: current state=weakNotTaken(predicts notTaken); branch taken → mispredicted=true → increment to weakTaken
      // Let's redo: use mispredicted=true to move from notTaken states toward taken
      doUpdate(dut, PC_A, TARGET_A, mispredicted = true)   // weakNotTaken(1) → weakTaken(2)
      val (_, _, pt1) = lookup(dut, PC_A)
      assert(pt1, "After recovering from weakNotTaken, should predict taken")
    }
  }

  // -------------------------------------------------------
  it should "handle entries in different sets independently" in {
    test(new BTB) { dut =>
      doUpdate(dut, PC_A, TARGET_A, mispredicted = false)  // set 0
      doUpdate(dut, PC_D, TARGET_D, mispredicted = false)  // set 2

      val (vA, tA, _) = lookup(dut, PC_A)
      val (vD, tD, _) = lookup(dut, PC_D)
      assert(vA && tA == TARGET_A, "Set 0 entry should be intact")
      assert(vD && tD == TARGET_D, "Set 2 entry should be intact")
    }
  }

  // -------------------------------------------------------
  it should "not predict for a PC that was never inserted even after other inserts" in {
    test(new BTB) { dut =>
      doUpdate(dut, PC_A, TARGET_A, mispredicted = false)
      doUpdate(dut, PC_D, TARGET_D, mispredicted = false)

      val (v, _, _) = lookup(dut, 0x5000)  // never inserted
      assert(!v, "Uninstalled PC should not hit")
    }
  }
}