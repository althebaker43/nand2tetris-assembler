package assembler

import org.scalatest.flatspec.AnyFlatSpec
import scala.io.Source

class AssemblerSpec extends AnyFlatSpec {

  val listing = """
   @OUTPUT_D
   0;JMP            // goto output_d
(OUTPUT_FIRST)
   @R0             
   D=M              // D = first number
(OUTPUT_D)
   @2
   M=D              // M[2] = D (greatest number)
(INFINITE_LOOP)
   @INFINITE_LOOP
   0;JMP            // infinite loop
   @R1
"""

  val labels = Map("OUTPUT_FIRST" -> 2, "OUTPUT_D" -> 4, "INFINITE_LOOP" -> 6)

  "The assembler" should "find the labels" in {

    val lineIter = Source.fromString(listing).getLines()
    val labels = Assembler.getLabels(lineIter, 1, 0)

    assert(labels.isEmpty == false)
    assert(labels.contains("OUTPUT_FIRST"))
    assert(labels.contains("OUTPUT_D"))
    assert(labels.contains("INFINITE_LOOP"))
    assert(labels.size == 3)
    assert(labels("OUTPUT_FIRST") == 2)
    assert(labels("OUTPUT_D") == 4)
    assert(labels("INFINITE_LOOP") == 6)
  }

  it should "assemble A instructions" in {

    val lineIter = Source.fromString(listing).getLines()
    val instrs = Assembler.getInstructions(srcLineIter=lineIter, labels=labels, addrs=Map(), instrs=Nil)
    val instrArr = Array.from(instrs)

    assert(instrArr.length == 9)
    assert(instrArr(0) == "0000000000000100")
    assert(instrArr(2) == "0000000000000000")
    assert(instrArr(4) == "0000000000000010")
    assert(instrArr(6) == "0000000000000110")
    assert(instrArr(8) == "0000000000000001")
  }
}
