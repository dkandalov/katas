package katas.kotlin.adventofcode.day3

import datsok.*
import org.junit.jupiter.api.*

class Tests {
    @Test fun `conversion from moves to positions`() {
        //...........
        //...........
        //...........
        //....+----+.
        //....|....|.
        //....|....|.
        //....|....|.
        //.........|.
        //.o-------+.
        //...........
        movesToPositions(listOf("R8", "U5", "L5", "D3")) shouldEqual listOf(
            Position(x = 0, y = 0),
            Position(x = 1, y = 0),
            Position(x = 2, y = 0),
            Position(x = 3, y = 0),
            Position(x = 4, y = 0),
            Position(x = 5, y = 0),
            Position(x = 6, y = 0),
            Position(x = 7, y = 0),
            Position(x = 8, y = 0),

            Position(x = 8, y = -1),
            Position(x = 8, y = -2),
            Position(x = 8, y = -3),
            Position(x = 8, y = -4),
            Position(x = 8, y = -5),

            Position(x = 7, y = -5),
            Position(x = 6, y = -5),
            Position(x = 5, y = -5),
            Position(x = 4, y = -5),
            Position(x = 3, y = -5),

            Position(x = 3, y = -4),
            Position(x = 3, y = -3),
            Position(x = 3, y = -2)
        )
    }

    @Test fun `steps to first intersection`() {
        // ...........
        //.+-----+...
        //.|.....|...
        //.|..+--X-+.
        //.|..|..|.|.
        //.|.-X--+.|.
        //.|..|....|.
        //.|.......|.
        //.o-------+.
        //...........
        countStepsToFirstIntersection(
            movesToPositions(listOf("R8", "U5", "L5", "D3")),
            movesToPositions(listOf("U7", "R6", "D4", "L4"))
        ) shouldEqual 30
    }
}