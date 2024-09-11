package assembler

import org.scalatest.flatspec.AnyFlatSpec
import scala.io.Source

class AssemblerSpec extends AnyFlatSpec {

  "The assembler" should "find the labels" in {
    val listing = """
   @OUTPUT_D
   0;JMP            // goto output_d
(OUTPUT_FIRST)
   @R0             
   D=M              // D = first number
(OUTPUT_D)
   @R2
   M=D              // M[2] = D (greatest number)
(INFINITE_LOOP)
   @INFINITE_LOOP
   0;JMP            // infinite loop
"""

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
}
