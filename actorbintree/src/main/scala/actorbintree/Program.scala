package actorbintree

import akka.actor.{Actor, Props, ActorSystem}
import actorbintree.BinaryTreeSet.{Remove, Contains, GC, Insert}

object Program {
  def main(args: Array[String]): Unit = {
    val actorSystem = ActorSystem("test")
    val set = actorSystem.actorOf(Props[BinaryTreeSet])
    val printer = actorSystem.actorOf(Props[Printer])

    set ! Insert(printer, 0, 1)
    set ! Insert(printer, 1, -1)
    set ! Insert(printer, 2, 2)
    set ! Contains(printer, 3, 2)
    set ! Remove(printer, 4, 2)
    set ! Contains(printer, 5, 2)
    //set ! GC
    readLine()
    actorSystem.shutdown()
  }
}

class Printer extends Actor {
  def receive: Actor.Receive = {
    case _ => println
  }
}
