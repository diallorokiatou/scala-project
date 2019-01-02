package fr.upem.main
import scala.io.Source
import fr.upem.model._
import fr.upem.controller._

import scala.collection.mutable.ListBuffer


object Main extends App {

  def initPelouse(file: String) : Option[Pelouse] = {
    val p =file.split(" ")
    if(p.length == 2) Some(new Pelouse(p(0).toInt,p(1).toInt,ListBuffer.empty))
    else None
  }

 def initTondeuses(liste:List[String],pelouse: Option[Pelouse]) : List[Tondeuse]= pelouse match
  {
   case None => Nil
   case _ =>   liste.zipWithIndex
               .filter{ case(s,i) => i%2 !=0 }
               .map{case(s,i) => s }
               .map(x => x.split(" "))
              .filter(_.length==3)
              .map(x => new Tondeuse(x(0).toInt,x(1).toInt,Orientation.getOrientation(x(2))))
 }

  def initMouvement(liste:List[String],pelouse: Option[Pelouse]) : List[List[Mouvement]]=  pelouse match
  {
    case None => Nil
    case _ => liste.zipWithIndex
             .filter{ case(s,i) => i!=0 && i%2 ==0 }
             .map{case(s,i) => s }
             .map(x => x.toList)
             .map{x => Mouvement.setMouvement(x)}
  }

 //lecture du fichier
  val l = Source.fromFile("ressources/config").getLines.toList
  if(l.nonEmpty) {
    //Propriétés
    println("********************Configuration*********************")
    val pelouseOption = initPelouse(l(0))
    if(pelouseOption == Nil) println("Erreur : Veuillez Vérifier votre fichier de configuration!")
    val pelouse : Pelouse = pelouseOption match {
      case Some(x) => x
      case None => new Pelouse(0,0,ListBuffer.empty)
    }
    val tondeuses : List[Tondeuse] = initTondeuses(l,pelouseOption)
    val mouvements : List[List[Mouvement]] = initMouvement(l,pelouseOption)
    println("pelouse : " + pelouse)
    println("tondeuses : " + tondeuses)
    println("mouvements : " + mouvements)
    println("******************Mouvements***********************")
    //--------------------deplacement ---------------------
    if(tondeuses!=Nil && mouvements!=Nil && tondeuses.size == mouvements.size) {
      for( i <- 0 to tondeuses.size - 1) pelouse.addTondeuse(TondeuseAction.deplacement(tondeuses(i),mouvements(i),pelouse))
    }
    else println("Erreur : Veuillez Vérifier votre fichier de configuration!")
    println("*******************Tondeuses sur la Pelouse**********************")
    println(pelouse.print(pelouse.tondeuses.toList))
  }

}
