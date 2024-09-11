package assembler

import scala.io.Source
import java.io.File

object Assembler {

  def main(args : Array[String]) : Unit = {
    val labels = getLabels(Source.fromFile(File(args(0))).getLines(), 1, 0)
    print(labels)

  }

  def getLabels(srcLineIter : Iterator[String], lineNum : Int, pc : Int) : Map[String, Int] = {
    if srcLineIter.hasNext == false then
      return Map()
    val srcLine = srcLineIter.next().strip()
    val newLabel = if srcLine.startsWith("(") then Map(srcLine.substring(1, srcLine.length()-1) -> pc) else Map()
    val newPC = if isInstruction(srcLine) then pc+1 else pc
    newLabel ++ getLabels(srcLineIter, lineNum+1, newPC)
  }

  def isInstruction(line : String) : Boolean = {
    val strippedLine = line.strip()
    if strippedLine.isEmpty() then
      false
    else if strippedLine.startsWith("//") then
      false
    else if strippedLine.startsWith("(") then
      false
    else
      true
  }

  def getInstructions(srcLineIter : Iterator[String], labels : Map[String, Int], addrs : Map[String, Int], instrs : List[String]) : List[String] = {
    if srcLineIter.hasNext == false then return instrs.reverse
    val srcLine = srcLineIter.next().strip()
    val newInstr = if srcLine.startsWith("@") then getAInstruction(srcLine, labels, addrs) else getCInstruction(srcLine)
    getInstructions(srcLineIter, labels, addrs, newInstr :: instrs)
  }

  def getAInstruction(instrStr : String, labels : Map[String, Int], addrs : Map[String, Int]) : String = {
    "0000000000000000"
  }

  def getCInstruction(instrStr : String) : String = {
    "0000000000000000"
  }
}

