package assembler

import scala.io.Source
import java.io.File
import java.io.PrintWriter

object Assembler {

  def main(args : Array[String]) : Unit = {
    val predefinedLabels = Map(
      "SP" -> 0,
      "LCL" -> 1,
      "ARG" -> 2,
      "THIS" -> 3,
      "THAT" -> 4,
      "SCREEN" -> 0x4000,
      "KBD" -> 0x6000
    )
    val registers = for i <- 0 to 15 yield (("R" + i.toString()) -> i)
    val labels = getLabels(Source.fromFile(File(args(0))).getLines(), 1, 0, predefinedLabels ++ registers.toMap)
    val instrs = getInstructions(Source.fromFile(File(args(0))).getLines(), labels, addrs=Map(), instrs=Nil)
    val instrWriter = PrintWriter(args(1))
    for instr <- instrs do instrWriter.println(instr)
    instrWriter.close()
  }

  def getLabels(srcLineIter : Iterator[String], lineNum : Int, pc : Int, labels : Map[String, Int]) : Map[String, Int] = {
    if srcLineIter.hasNext == false then
      return labels
    val srcLine = srcLineIter.next().strip()
    val newLabel = if srcLine.startsWith("(") then Map(srcLine.substring(1, srcLine.length()-1) -> pc) else Map()
    val newPC = if isInstruction(srcLine) then pc+1 else pc
    getLabels(srcLineIter, lineNum+1, newPC, newLabel ++ labels)
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
    val instrStr = if srcLine.contains("//") then
      srcLine.substring(0, srcLine.indexOf("//")).strip()
    else
      srcLine
    val newAddrs = if (isInstruction(instrStr) && instrStr.startsWith("@")) then
      getNewAddresses(instrStr, labels, addrs)
    else
      addrs
    val newInstrs = if isInstruction(instrStr) then {
      if instrStr.startsWith("@") then
        getAInstruction(instrStr, labels, newAddrs) :: instrs
      else
        getCInstruction(instrStr) :: instrs
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
    else if addrs.contains(target) then
      addrs
    else
      try {
        target.toInt
        addrs
      } catch
        case e : NumberFormatException => addrs + (target -> (addrs.size + 0x0010))
  }

  def getCInstruction(instrStr : String) : String = {
    val tokens = instrStr.split(";")
    val assignTokens = tokens(0).split("=")
    val compTokenIdx = if assignTokens.length > 1 then 1 else 0
    val destField = if assignTokens.length > 1 then {
      assignTokens(0) match {
        case "D"   => "010"
        case "M"   => "001"
        case "MD"  => "011"
        case "A"   => "100"
        case "AM"  => "101"
        case "AD"  => "110"
        case "AMD" => "111"
        case _     => "000"
      }
    }
    else
      "000"
    val acFields = assignTokens(compTokenIdx) match {
      case "0"   => "0101010"
      case "1"   => "0111111"
      case "-1"  => "0111010"
      case "D"   => "0001100"
      case "A"   => "0110000"
      case "!D"  => "0001101"
      case "!A"  => "0110001"
      case "-D"  => "0001111"
      case "-A"  => "0110011"
      case "D+1" => "0011111"
      case "A+1" => "0110111"
      case "D-1" => "0001110"
      case "A-1" => "0110010"
      case "D+A" => "0000010"
      case "D-A" => "0010011"
      case "A-D" => "0000111"
      case "D&A" => "0000000"
      case "D|A" => "0010101"
      case "M"   => "1110000"
      case "!M"  => "1110001"
      case "-M"  => "1110011"
      case "M+1" => "1110111"
      case "M-1" => "1110010"
      case "D+M" => "1000010"
      case "D-M" => "1010011"
      case "M-D" => "1000111"
      case "D&M" => "1000000"
      case "D|M" => "1010101"
      case _     => "0101010"
    }
    val jmpField = if tokens.length > 1 then {
      tokens(1) match {
        case "JGT" => "001"
        case "JEQ" => "010"
        case "JGE" => "011"
        case "JLT" => "100"
        case "JNE" => "101"
        case "JLE" => "110"
        case "JMP" => "111"
        case _     => "000"
      }
    }
    else
      "000"
    "111" + acFields + destField + jmpField
  }
}

