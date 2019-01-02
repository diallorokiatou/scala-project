package fr.upem.controller

import fr.upem.model.{Mouvement, Orientation, Pelouse, Tondeuse}

object TondeuseAction {

  def deplacement(tondeuse: Tondeuse,mouvement: List[Mouvement],pelouse: Pelouse) : Tondeuse = {
    mouvement match {
      case x::xs if(x==Mouvement.D) => deplacement(directionD(tondeuse),xs,pelouse)//directionD(tondeuse)
      case x::xs if(x==Mouvement.G) => deplacement(directionG(tondeuse),xs,pelouse)//directionG(tondeuse)
      case x::xs if(x==Mouvement.A) => deplacement(directionA(tondeuse,pelouse),xs,pelouse)//directionA(tondeuse,pelouse)
      case _ => tondeuse
    }
  }

  //Pour eviter toute collision on va tester si une tondeuse n'est pas déjà à la poition qu'on veut acceder dans ce cas la tondeusene bouge pas
  def directionA(t: Tondeuse,pelouse: Pelouse) : Tondeuse  = t.getO match {
    case Orientation.E if((pelouse.getX > t.getX) && (pelouse.testPosition(Tondeuse(t.x+1,t.y,t.o),pelouse.tondeuses.toList)))
          => Tondeuse(t.x+1,t.y,t.o)
    case Orientation.N if((pelouse.getY > t.getY)  && (pelouse.testPosition(Tondeuse(t.x,t.y+1,t.o),pelouse.tondeuses.toList)))
        => Tondeuse(t.x,t.y+1,t.o)
    case Orientation.S if((t.getY > 0) && (pelouse.testPosition(Tondeuse(t.x,t.y-1,t.o),pelouse.tondeuses.toList)))
        => Tondeuse(t.x,t.y-1,t.o)
    case Orientation.W if(t.getX > 0) && (pelouse.testPosition(Tondeuse(t.x-1,t.y,t.o),pelouse.tondeuses.toList))
        => Tondeuse(t.x-1,t.y,t.o)
    case _ => t
  }

  def directionD(t: Tondeuse) : Tondeuse = t.o match {
    case Orientation.S => Tondeuse(t.x,t.y,Orientation.W)
    case Orientation.N => Tondeuse(t.x,t.y,Orientation.E)
    case Orientation.W => Tondeuse(t.x,t.y,Orientation.N)
    case Orientation.E => Tondeuse(t.x,t.y,Orientation.S)
    case _ => t
  }

  def directionG(t: Tondeuse) : Tondeuse = t.o match {
    case Orientation.S => Tondeuse(t.x,t.y,Orientation.E)
    case Orientation.N => Tondeuse(t.x,t.y,Orientation.W)
    case Orientation.W => Tondeuse(t.x,t.y,Orientation.S)
    case Orientation.E => Tondeuse(t.x,t.y,Orientation.N)
    case _ => t
  }


}
