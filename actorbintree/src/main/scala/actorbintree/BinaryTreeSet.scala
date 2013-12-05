/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue
import akka.event.LoggingReceive

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case operation: Operation => root ! operation
    case GC => {
      val newRoot = createRoot
      context.become(garbageCollecting(newRoot))
      root ! CopyTo(newRoot)
    }
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case operation: Operation => pendingQueue = pendingQueue.enqueue(operation)
    case GC => {}
    case CopyFinished => {
      context.stop(root)
      root = newRoot
      context.become(normal)
      for (operation <- pendingQueue) {
        self ! operation
        pendingQueue = Queue.empty[Operation]
      }
    }
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case insert @ Insert(requester, id, e) => {
      if (e == elem) {
        if (removed)
          removed = false
        requester ! OperationFinished(id)
      } else {
        val position = if (e < elem) Left else Right
        val option = subtrees get position
        option match {
          case Some(node) =>
            node ! insert
          case None => {
            val node = context.actorOf(props(e, false))
            subtrees = subtrees + (position -> node)
            requester ! OperationFinished(id)
          }
        }
      }
    }
    case contains @ Contains(requester, id, e) => {
      if (e == elem)
        requester ! ContainsResult(id, !removed)
      else {
        val position = if (e < elem) Left else Right
        val option = subtrees get position
        option match {
          case Some(node) =>
            node ! contains
          case None =>
            requester ! ContainsResult(id, false)
        }
      }
    }
    case remove @ Remove(requester, id, e) => {
      if (e == elem) {
        removed = true
        requester ! OperationFinished(id)
      } else {
        val position = if (e < elem) Left else Right
        val option = subtrees get position
        option match {
          case Some(node) =>
            node ! remove
          case None =>
            requester ! OperationFinished(id)
        }
      }
    }
    case copyTo @ CopyTo(treeNode) => {
      if (!removed)
        treeNode ! Insert(self, elem, elem)
      val children = subtrees.values.toSet
      for (child <- children)
        child ! copyTo
      if (removed && children.isEmpty)
        context.parent ! CopyFinished
      else
        context.become(copying(children, removed))
    }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = LoggingReceive {
    case OperationFinished(id) => {
      if (expected.isEmpty)
        context.parent ! CopyFinished
      else
        context.become(copying(expected, true))
    }
    case CopyFinished => {
      val remaining = expected - sender
      if (remaining.isEmpty && insertConfirmed)
        context.parent ! CopyFinished
      else
        context.become(copying(remaining, insertConfirmed))
    }
  }
}
