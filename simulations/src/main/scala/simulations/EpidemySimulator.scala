package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val transmissibilityRate = 40
    val prevalenceRate = 1
    val maxDaysForMove = 5
    val incubationDays = 6
    val mortalityDays = 14 - incubationDays
    val mortalityRate = 25
    val immunityDays = 16 - mortalityDays
    val cureDays = 18
  }

  import SimConfig._

  object Direction extends Enumeration {
    case class Direction(row: Int, col: Int)
    val up = Direction(1, 0)
    val down = Direction(-1, 0)
    val left = Direction(0, -1)
    val right = Direction(0, 1)
    val directions = List(up, down, left, right)
  }

  val persons: List[Person] = (0 until population).map(new Person(_)).toList

  for (i <- 0 until population * prevalenceRate / 100) persons(i).becomeInfected

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def moveAfterDelay {
      val delay = randomBelow(maxDaysForMove)
      afterDelay(delay) {
        if (!dead) {
          move
          moveAfterDelay
        }
      }
    }

    def move {
      val valid = for {
        direction <- Direction.directions
        newRow = (row + direction.row + roomRows) % roomRows
        newCol = (col + direction.col + roomColumns) % roomColumns
        if (roomLooksSafe(newRow, newCol))
      } yield (newRow, newCol)
      if (!valid.isEmpty) {
        val (newRow, newCol) = valid(randomBelow(valid.length))
        row = newRow
        col = newCol
        enter
      }
    }

    def enter {
      if (!infected && roomInfected(row, col)) {
        exposed
      }
    }

    def exposed {
      if (!immune) {
        val transmitted = P(transmissibilityRate)
        if (transmitted) {
          becomeInfected
        }
      }
    }

    def becomeInfected {
      infected = true
      afterDelay(incubationDays) {
        sick = true
        afterDelay(mortalityDays) {
          val dies = P(mortalityRate)
          if (dies) {
            dead = true
          } else {
            afterDelay(immunityDays) {
              if (!dead) {
                sick = false
                immune = true
              }
            }
          }
        }
      }
    }

    def visiblyInfected = sick || dead

    def others = persons.filter(p => p.id != id)

    def inRoom(r: Int, c: Int) = others.filter(p => p.row == r && p.col == c)

    def roomInfected(r: Int, c: Int) = inRoom(r, c).exists(_.infected)

    def roomLooksSafe(r: Int, c: Int) = inRoom(r, c).forall(!_.visiblyInfected)

    def P(p: Int) = randomBelow(100) < p

    moveAfterDelay
  }
}
