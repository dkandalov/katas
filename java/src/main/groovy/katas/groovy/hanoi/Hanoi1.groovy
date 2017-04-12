package katas.groovy.hanoi

import org.junit.Test

/**
 * User: dima
 * Date: 7/2/11
 */
class Hanoi1 {
  final shouldFail = new GroovyTestCase().&shouldFail

  @Test
  public void shouldAllowValidMoves() {
    def hanoi = new HanoiGame()
    assert hanoi.state == [[], [], [1, 2, 3]]
    assert hanoi.move(2, 2).state == [[], [], [1, 2, 3]]
    assert hanoi.move(2, 0).state == [[1], [], [2, 3]]
    assert hanoi.move(2, 1).state == [[1], [2], [3]]
  }

  @Test
  public void shouldThrowExceptionIfMoveIsInvalid() {
    def hanoi = new HanoiGame()
    assert hanoi.state == [[], [], [1, 2, 3]]

    shouldFail { hanoi.move(0, 1) }
    assert hanoi.move(2, 0).state == [[1], [], [2, 3]]
    shouldFail { hanoi.move(2, 0) }
    assert hanoi.move(2, 1).state == [[1], [2], [3]]
    shouldFail { hanoi.move(2, 1) }
  }

  @Test
  public void masterShouldCorrectlyMoveAnyDisk() {
    def hanoi

    hanoi = new HanoiGame()
    new HanoiMaster(hanoi).moveDisk(1, 1)
    assert hanoi.state == [[], [1], [2, 3]] // failed to make it work from first attempt... forgot that "return" in closure doesn't exit method
    new HanoiMaster(hanoi).moveDisk(1, 2)
    assert hanoi.state == [[], [], [1, 2, 3]]
    new HanoiMaster(hanoi).moveDisk(1, 0)
    assert hanoi.state == [[1], [], [2, 3]]

    hanoi = new HanoiGame()
    new HanoiMaster(hanoi).moveDisk(2, 1)
    assert hanoi.state == [[], [1, 2], [3]]
    new HanoiMaster(hanoi).moveDisk(2, 2)
    assert hanoi.state == [[], [], [1, 2, 3]]

    hanoi = new HanoiGame()
    new HanoiMaster(hanoi).moveDisk(2, 0)
    assert hanoi.state == [[1, 2], [], [3]]

    hanoi = new HanoiGame()
    new HanoiMaster(hanoi).moveDisk(3, 0)
    assert hanoi.state == [[1, 2, 3], [], []]
  }

  private static class HanoiMaster {
    HanoiGame hanoi

    HanoiMaster(HanoiGame hanoi) {
      this.hanoi = hanoi
    }

    def moveDisk(int disk, int toPost) {
      def position = find(disk)
      def diff = (toPost - position[0]) // failed to write correct diff from first attempt (found before running)
      if (diff < -1) diff = 1
      if (diff > 1) diff = -1

      shiftDisk(disk, (int) diff)
    }

    def shiftDisk(int disk, int shift) {
      def position = find(disk)
      if (position[1] == 0) {
        hanoi.move(position[0], absPosition(position[0] + shift))
        return
      }

      def diskAbove = hanoi.state[position[0]][position[1] - 1] // only used index for posts.. like this "state[position[1] - 1]"
      shiftDisk(diskAbove, -shift)
      shiftDisk(disk, shift)
      shiftDisk(diskAbove, -shift)
    }

    def absPosition(def post) {
      if (post < 0) return post + 3 // was 3 - post instead of post + 3
      if (post >= 3) return post - 3 // was > instead of >=
      post
    }

    def find(int disk) {
      def postIndex = hanoi.state.findIndexOf { post ->
        post.indexOf(disk) != -1
      }
      if (postIndex == -1) throw new IllegalStateException()
      [postIndex, hanoi.state[postIndex].indexOf(disk)]
    }
  }

  private static class HanoiGame {
    def state

    HanoiGame() {
      state =
        [
                [],
                [],
                [1, 2, 3]
        ]
    }

    def move(fromPost, toPost) {
      if (state.empty) throw new IllegalStateException()

      def disk = state[fromPost].get(0) // didn't consider that removing and then validating state may corrupt object's internal state
      def allowedToMove = (state[toPost].empty || state[toPost][0] >= disk)
      if (!allowedToMove) throw new IllegalStateException()

      state[toPost].add(0, disk)
      state[fromPost].remove(0)

      this
    }
  }
}
