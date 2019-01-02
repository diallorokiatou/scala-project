package fr.upem.model

import scala.collection.mutable.ListBuffer
import fr.upem.model._

class Pelouse(x : Int,y : Int, var tondeuses: ListBuffer[Tondeuse]) {

  object Pelouse {
    def apply: Pelouse = new Pelouse(x,y,ListBuffer.empty)

    def unapply(arg: Pelouse): Option[Tondeuse] = None
}

  def getX : Int = x
  def getY : Int = y

  def addTondeuse(tondeuse : Tondeuse)  =  {
    if(testPosition(tondeuse,tondeuses.toList)) tondeuses += tondeuse
    else println(s"Risque de collision. Veuillez vérifier le fichier de configuration svp!")
  }

  def testPosition(tondeuse : Tondeuse,tondeuses: List[Tondeuse]) : Boolean =
    tondeuses match {
      case Nil => true
      case x :: xs if(x == tondeuse) => false
      case x :: xs if x != tondeuse => testPosition(x, xs)
    }

  override def toString: String = s"Pelouse($x,$y)"

  def print(tondeuses: List[Tondeuse]) : String = tondeuses.toList match {
    case Nil => ""
    case x:: xs =>   s"$x" + "\n" + print(xs)

  }
}
