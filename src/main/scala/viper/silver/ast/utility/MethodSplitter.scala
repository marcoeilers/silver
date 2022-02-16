package viper.silver.ast.utility

import viper.silver.ast.{And, DomainFuncApp, Exp, Method, Not, Program, TrueLit}

import scala.collection.mutable

object MethodSplitter {

  val splitFuncName = "____splitOn"

  def containsSplit(m: Method) : Boolean = {
    m.existsDefined{
      case DomainFuncApp(this.splitFuncName, args, _) => true
    }
  }

  def split(p: Program) : Program = {
    val methodsToSplit = p.methods.filter(containsSplit)
    val updatedMethods: Seq[Method] = methodsToSplit.flatMap(splitMethod)
    val unchangedMethods = p.methods.filter(m => !methodsToSplit.contains(m))
    p.copy(methods = updatedMethods ++ unchangedMethods)(p.pos, p.info, p.errT)
  }

  def splitMethod(m: Method) : Seq[Method] = {
    var methodsToDo = mutable.Seq(m)
    var result: mutable.Seq[Method] = mutable.Seq()
    result ++= Seq(m.transform{
      case DomainFuncApp(this.splitFuncName, args, _) => TrueLit()()
    }.copy(body = None)(m.pos, m.info, m.errT))
    while (!methodsToDo.isEmpty) {
      val current = methodsToDo.head
      methodsToDo = methodsToDo.tail

      val splitExp = current.find({
        case DomainFuncApp(this.splitFuncName, args, _) => true
        case _ => false
      })
      if (splitExp.isDefined){
        val conditions = generateConditions(splitExp.get.asInstanceOf[Exp])
        val newVersions = conditions.zipWithIndex.map{case (c, i) => current.replace(splitExp.get, c).copy(name = current.name+"_"+i)(current.pos, current.info, current.errT)}
        val newVersionsTodo = newVersions.filter(containsSplit)
        val newVersionsFinished = newVersions.filterNot(containsSplit)
        methodsToDo ++= newVersionsTodo
        result ++= newVersionsFinished
      }
    }
    result.toSeq
  }

  def generateConditions(dfa: Exp) : Seq[Exp] = {
    dfa match {
      case tl@TrueLit() => Seq(tl)
      case DomainFuncApp(this.splitFuncName, Seq(cond, ifTrue, ifFalse), _) => {
        val ifTrueConditions = generateConditions(ifTrue)
        val ifFalseConditions = generateConditions(ifFalse)
        val newIfTrue = ifTrueConditions.map(c => And(cond, c)(cond.pos, cond.info, cond.errT))
        val newIfFalse = ifFalseConditions.map(c => And(Not(cond)(cond.pos, cond.info, cond.errT), c)(cond.pos, cond.info, cond.errT))
        newIfTrue ++ newIfFalse
      }
    }
  }

}
