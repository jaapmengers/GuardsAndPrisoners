package com.jmengers.GuardsAndPrisoners

import scala.collection.immutable._

sealed trait Direction
case object Left extends Direction
case object Right extends Direction

sealed trait Move {
  implicit val m1 = Bag.configuration.compact[Node]
  val direction: Direction
  val toBeMoved: Bag[Node]
}
case class MoveOneGuard(direction: Direction) extends Move {
  val toBeMoved: Bag[Node] = Bag(Guard)
}
case class MoveTwoGuards(direction: Direction) extends Move {
  val toBeMoved: Bag[Node] = Bag(Guard, Guard)
}
case class MoveOnePrisoner(direction: Direction) extends Move {
  val toBeMoved: Bag[Node] = Bag(Prisoner)
}
case class MoveTwoPrisoners(direction: Direction) extends Move {
  val toBeMoved: Bag[Node] = Bag(Prisoner, Prisoner)
}
case class MoveOneGuardAndOnePrisoner(direction: Direction) extends Move {
  val toBeMoved: Bag[Node] = Bag(Guard, Prisoner)
}

sealed trait Node
case object Guard extends Node
case object Prisoner extends Node
case object Boat extends Node

case class State(leftBank: Bag[Node], rightBank: Bag[Node]) {

  def applyMove(m: Move) = m.direction match {
    case Right if m.toBeMoved.subsetOf(leftBank) => State((leftBank - Boat) -- m.toBeMoved, rightBank ++ m.toBeMoved + Boat)
    case Left if m.toBeMoved.subsetOf(rightBank) => State(leftBank ++ m.toBeMoved + Boat, (rightBank - Boat) -- m.toBeMoved)
    case _ => this
  }
}

object Solver {

  implicit val m1 = Bag.configuration.compact[Node]

  def main(args: Array[String]): Unit = {

    val beginState = State(leftBank = Bag(Guard, Guard, Guard, Prisoner, Prisoner, Prisoner, Boat), rightBank = Bag.empty[Node])

    val endState = State(leftBank = Bag.empty[Node], rightBank = Bag(Guard, Guard, Guard, Prisoner, Prisoner, Prisoner, Boat))

    val answer = solve(beginState, endState)
    println(s"The answer is: $answer")
  }

  def solve(beginState: State, endState: State): List[Move] = solveRecursively(beginState, endState, Set(beginState), Nil)

  def solveRecursively(curState: State, endState: State, previousStates: Set[State], moves: List[Move]): List[Move] = (curState, endState) match {
    case (cur, end) if cur == end => moves
    case (cur, end) =>
      val states = Generator.generate(cur, previousStates)
      val (correct, uncorrect) = states.partition(x => Tester.test(x._2))
      val res = correct.map(x => solveRecursively(x._2, end, previousStates ++ states.map(_._2), moves :+ x._1))

      val amounts = res.filter(_.nonEmpty)

      if(amounts.isEmpty) Nil else amounts.minBy(_.size)
  }
}

object Generator {

  def getMovesForDirection(direction: Direction) = Set(
    MoveOneGuard(direction),
    MoveTwoGuards(direction),
    MoveOnePrisoner(direction),
    MoveTwoPrisoners(direction),
    MoveOneGuardAndOnePrisoner(direction)
  )

  def generate(state: State, previousStates: Set[State]): Set[(Move, State)] = {
    val direction = if(state.leftBank.contains(Boat)) Right else Left
    val moves = getMovesForDirection(direction)

    val newStates: Set[(Move, State)] = moves.map(x => x -> state.applyMove(x))

    newStates.filterNot(x => previousStates.contains(x._2) )
  }
}

object Tester {
  def test(s: State): Boolean = List(s.leftBank.multiplicity(Guard), s.leftBank.multiplicity(Prisoner), s.rightBank.multiplicity(Guard), s.rightBank.multiplicity(Prisoner)) match {
    // There's no guards on the left bank. Ok
    case List(0, _, _, _) => true
    // There's no guards an the right bank. Ok
    case List(_, _, 0, _) => true
    // There's more prisoners than guards on the left bank
    case List(a, b, _, _) if b > a => false
    // There's more prisoners that guards and the right bank
    case List(_, _, a, b) if b > a => false
    case _ => true
  }
}