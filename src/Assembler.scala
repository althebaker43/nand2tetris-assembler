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
    val newAddrs = if (isInstruction(srcLine) && srcLine.startsWith("@")) then
      getNewAddresses(srcLine, labels, addrs)
    else
      addrs
    val newInstrs = if isInstruction(srcLine) then {
      if srcLine.startsWith("@") then
        getAInstruction(srcLine, labels, newAddrs) :: instrs
      else
        getCInstruction(srcLine) :: instrs
    } else {
      instrs
    }
    getInstructions(srcLineIter, labels, newAddrs, newInstrs)
  }

  def getAddrStr(addr : Int, bitIdx : Int) : String = {
    if bitIdx == 0 then
      s"${addr & 1}"
    else {
      val maskedAddr = addr & (1 << bitIdx)
      val newBit = if maskedAddr == 0 then
        "0"
      else
        "1"
      newBit + getAddrStr(addr, bitIdx-1)
    }
  }

  def getAInstruction(instrStr : String, labels : Map[String, Int], addrs : Map[String, Int]) : String = {
    val label = instrStr.substring(1)
    val labelAddr = if labels.contains(label) then
      labels(label)
    else if addrs.contains(label) then
      addrs(label)
    else
      label.toInt
    "0" + getAddrStr(labelAddr, 14)
  }

  def getNewAddresses(instrStr : String, labels : Map[String, Int], addrs : Map[String, Int]) : Map[String, Int] = {
    val target = instrStr.substring(1)
    if labels.contains(target) then
      addrs
    else
      try {
        target.toInt
        addrs
      } catch
        case e : NumberFormatException => addrs + (target -> addrs.size)
  }

  def getCInstruction(instrStr : String) : String = {
    "0000000000000000"
  }
}

