package org.bitcoins.script.stack

import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/6/16.
 */
class StackOperationsTest extends FlatSpec with MustMatchers {

  "StackOperations" must "define OP_TOALTSTACK" in {
    OP_TOALTSTACK.opCode must be (107)
  }

  it must "define OP_FROMALTSTACK" in {
    OP_FROMALTSTACK.opCode must be (108)
  }

  it must "define OP_IFDUP" in {
    OP_IFDUP.opCode must be (115)
  }

  it must "define OP_DEPTH" in {
    OP_DEPTH.opCode must be (116)
  }

  it must "define OP_DROP" in {
    OP_DROP.opCode must be (117)
  }
  it must "define OP_DUP" in {
    OP_DUP.opCode must be (118)
  }

  it must "define OP_NIP" in {
    OP_NIP.opCode must be (119)
  }

  it must "define OP_OVER" in {
    OP_OVER.opCode must be (120)
  }

  it must "define OP_PICK" in {
    OP_PICK.opCode must be (121)
  }

  it must "define OP_ROLL" in {
    OP_ROLL.opCode must be (122)
  }

  it must "define OP_ROT" in {
    OP_ROT.opCode must be (123)
  }

  it must "define OP_SWAP" in {
    OP_SWAP.opCode must be (124)
  }

  it must "define OP_TUCK" in {
    OP_TUCK.opCode must be (125)
  }

  it must "define OP_2DROP" in {
    OP_2DROP.opCode must be (109)
  }

  it must "define OP_2DUP" in {
    OP_2DUP.opCode must be (110)
  }

  it must "define OP_3DUP" in {
    OP_3DUP.opCode must be (111)
  }

  it must "define OP_2OVER" in {
    OP_2OVER.opCode must be (112)
  }

  it must "define OP_2ROT" in {
    OP_2ROT.opCode must be (113)
  }

  it must "define OP_2SWAP" in {
    OP_2SWAP.opCode must be (114)
  }


}
