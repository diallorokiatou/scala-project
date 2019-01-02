package fr.upem.model




sealed trait Orientation

object Orientation {

  case object N extends Orientation

  case object W extends Orientation

  case object S extends Orientation

  case object E extends Orientation

  def getOrientation(s: String) : Orientation = s match {
    case "N" => Orientation.N
    case "W" => Orientation.W
    case "S" => Orientation.S
    case "E" => Orientation.E
  }
}

sealed trait Mouvement

object Mouvement {

  case object A extends Mouvement

  case object D extends Mouvement

  case object G extends Mouvement

  def getMouvement(s: String) : Mouvement = s match {
    case "A" => Mouvement.A
    case "D" => Mouvement.D
    case "G" => Mouvement.G
  }

  def setMouvement(c: List[Char]) : List[Mouvement] = c match {
    case x::xs =>getMouvement(""+x)::setMouvement(xs)
    case Nil => Nil
  }

}

case class Tondeuse( x : Int,  y : Int, o: Orientation) {

  object Tondeuse{
     def apply(x : Int,  y : Int, o: Orientation): Tondeuse = new Tondeuse(x,y,o)
}

  def getX : Int = x
  def getY : Int = y
  def getO : Orientation = o

  override def toString: String = s"Tondeuse($x  , $y , $o )"

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case that:Tondeuse => this.x ==that.x && this.y ==that.y && this.o == that.o
      case _ => false
    }

}

