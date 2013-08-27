package org.lscc.tictactoe

import org.specs2.mutable.Specification

class MainTest extends Specification {
	 "Tic-tac-Toe bot robot" should {
		 "play a move" in {
			 Main.nextMove("---------") must be_==(4)
			 Main.nextMove("0--------") must be_==(4)
			 Main.nextMove("----0----") must be_==(6)
			 Main.nextMove("---x0----") must be_==(6)

			 Main.nextPlayer("---------") must be_==("0")
			 Main.nextPlayer("0--------") must be_==("x")
			 Main.nextPlayer("0x-------") must be_==("0")
		 }

		 "not lose" in {
			 // horizontal
			 Main.nextMove("00--x----") must be_==(2)
			 Main.nextMove("0-0-x----") must be_==(1)
			 Main.nextMove("-00-x----") must be_==(0)

			 Main.nextMove("---00--x-") must be_==(5)
			 Main.nextMove("---0-0-x-") must be_==(4)
			 Main.nextMove("----00-x-") must be_==(3)

			 // vertical
			 Main.nextMove("0--0x----") must be_==(6)
			 Main.nextMove("-0-x0----") must be_==(7)
			 Main.nextMove("--0x-0---") must be_==(8)

			 Main.nextMove("--0--0-x-") must be_==(8)
			 Main.nextMove("--0----x0") must be_==(5)
			 Main.nextMove("-----0-x0") must be_==(2)

			 Main.nextMove("----0--x0") must be_==(0)
			 Main.nextMove("-x0-0----") must be_==(6)
			 Main.nextMove("0x------0") must be_==(4)
		 }
	 }
 }