package ru.birds

import java.awt.Dimension
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.event.MouseEvent
import java.awt.event.MouseMotionAdapter
import java.awt.geom.AffineTransform
import java.awt.geom.Point2D
import javax.swing.JFrame
import javax.swing.JPanel
import org.junit.Test
import static java.lang.Math.*
import static junit.framework.Assert.assertEquals

/**
 * User: dima
 * Date: 28/10/2011
 */
class Main0 {
  public static void main(String[] args) {
    def frame = new JFrame()
    frame.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
    frame.add(new MyPanel(Bird.createFlock()))
    frame.preferredSize = new Dimension(800, 800)

    frame.pack()
    frame.visible = true

    new Thread({
      while (true) {
        frame.repaint()
        Thread.sleep(100)
      }
    }).start()
  }
}

class Bird {

  static List<Bird> createFlock() {
    def leader = new Bird(300, 300, 90, 10)
    [leader] + createFollowersFor(leader, 10)
  }

  static List<Bird> createFollowersFor(Bird leader, int count) {
    if (count == 0) return []
    def bird1 = new Bird(leader.x - 20, leader.y - 10, leader.direction, leader.speed, leader)
    def bird2 = new Bird(leader.x - 20, leader.y + 10, leader.direction, leader.speed, leader)
    [bird1, bird2] + createFollowersFor(bird1, count - 1) + createFollowersFor(bird2, count - 1)
  }

  Bird leader
  double x, y
  private double direction
  double speed
  double leaderXDiff
  double leaderYDiff

  Bird(double x, double y, double direction, double speed, Bird leader = null) {
    this.x = x
    this.y = y
    setDirection(direction)
    this.speed = speed
    this.leader = leader

    if (leader != null) {
      def transform = AffineTransform.getRotateInstance(toRadians(leader.direction), 0, 0)
      def result = new Point2D.Double()
      transform.transform(new Point2D.Double(x - leader.x, y - leader.y), result)
      this.leaderXDiff = result.x
      this.leaderYDiff = result.y
    }
  }

  def update() {
    if (leader != null) {
/*
      if (abs(leader.direction - direction) < 180) {
        direction = (leader.direction + direction) / 2
      } else {
        direction = normalized((leader.direction + direction + 360) / 2)
      }
*/
      def transform = new AffineTransform()
      transform.rotate(toRadians(-leader.direction), leader.x, leader.y)
//      transform.translate(leader.x, leader.y)
      def result = new Point2D.Double()
      transform.transform(new Point2D.Double(leaderXDiff + leader.x, leaderYDiff + leader.y), result)
      moveTowards(result.x, result.y)
//      x = result.x
//      y = result.y
    }
    x += sin(toRadians(direction)) * speed
    y += cos(toRadians(direction)) * speed
  }

  def moveTowards(double targetX, double targetY) {
    def xDiff = (targetX - x)
    def yDiff = (targetY - y)
    if (xDiff >= 0 && yDiff < 0) {
      setDirection(180 - toDegrees(atan(xDiff / -yDiff)))
    } else if (xDiff < 0 && yDiff < 0) {
      setDirection(180 + toDegrees(atan(-xDiff / -yDiff)))
    } else if (xDiff < 0 && yDiff >= 0) {
      setDirection(360 - toDegrees(atan(-xDiff / yDiff)))
    } else if (xDiff >= 0 && yDiff >= 0) {
      setDirection(toDegrees(atan(xDiff / yDiff)))
    }
  }

  def getDirection() {
    direction
  }

  def setDirection(double value) {
    this.direction = normalized(value)
  }

  static normalized(double direction) {
    direction % 360
  }
}

class BirdTest {
  @Test public void shouldMoveTowardsWithCorrectDirection() {
    assertMovesTowardsPoint(new Bird(0, 0, 0, 10), 0, 10, 0)
    assertMovesTowardsPoint(new Bird(0, 0, 0, 10), 10, 10, 45)
    assertMovesTowardsPoint(new Bird(0, 0, 0, 10), 10, 0, 90)
    assertMovesTowardsPoint(new Bird(0, 0, 0, 10), 10, -10, 135)
    assertMovesTowardsPoint(new Bird(0, 0, 0, 10), 0, -10, 180)
    assertMovesTowardsPoint(new Bird(0, 0, 0, 10), -10, -10, 225)
    assertMovesTowardsPoint(new Bird(0, 0, 0, 10), -10, 0, 270)
    assertMovesTowardsPoint(new Bird(0, 0, 0, 10), -10, 10, 315)

    assertMovesTowardsPoint(new Bird(0, 0, 0, 10), 5, 10, 26.56)
    assertMovesTowardsPoint(new Bird(0, 0, 0, 10), 10, 5, 90 - 26.56)
    assertMovesTowardsPoint(new Bird(0, 0, 0, 10), 10, -5, 90 + 26.56)
    assertMovesTowardsPoint(new Bird(0, 0, 0, 10), 5, -10, 180 - 26.56)
    assertMovesTowardsPoint(new Bird(0, 0, 0, 10), -5, -10, 180 + 26.56)
    assertMovesTowardsPoint(new Bird(0, 0, 0, 10), -10, -5, 270 - 26.56)
    assertMovesTowardsPoint(new Bird(0, 0, 0, 10), -10, 5, 270 + 26.56)
    assertMovesTowardsPoint(new Bird(0, 0, 0, 10), -5, 10, 360 - 26.56)
  }

  static assertMovesTowardsPoint(Bird bird, double x, double y, double angle) {
    bird.moveTowards(x, y)
    assertEquals("", angle, bird.direction, 0.01)
  }

  @Test public void shouldGetCloserToLeader() {
    def birdWithLeader = {direction, leaderDirection ->
      new Bird(0, 0, direction, 10, new Bird(50, 50, leaderDirection, 10))
    }
    assertNextAngle(birdWithLeader(0, 60), 30)
    assertNextAngle(birdWithLeader(60, 120), 90)
    assertNextAngle(birdWithLeader(120, 200), 160)
    assertNextAngle(birdWithLeader(220, 340), 280)

    assertNextAngle(birdWithLeader(60, 0), 30)
    assertNextAngle(birdWithLeader(120, 60), 90)

    assertNextAngle(birdWithLeader(40, 340), 10)
    assertNextAngle(birdWithLeader(340, 40), 10)
    assertNextAngle(birdWithLeader(0, 180), 270)
    assertNextAngle(birdWithLeader(180, 0), 270)
    assertNextAngle(birdWithLeader(90, 270), 0)
    assertNextAngle(birdWithLeader(270, 90), 0)
  }

  static assertNextAngle(Bird bird, double direction) {
    bird.update()
    assertEquals("", direction, bird.direction, 0.01)
  }

  @Test public void shouldMoveAfterEachUpdate() {
    assertNextStepOf(new Bird(0, 0, 0, 10), 0, 10)
    assertNextStepOf(new Bird(0, 0, 45, 10), sqrt(200) / 2, sqrt(200) / 2)
    assertNextStepOf(new Bird(0, 0, 90, 10), 10, 0)
    assertNextStepOf(new Bird(0, 0, 135, 10), sqrt(200) / 2, -sqrt(200) / 2)
    assertNextStepOf(new Bird(0, 0, 180, 10), 0, -10)
    assertNextStepOf(new Bird(0, 0, 225, 10), -sqrt(200) / 2, -sqrt(200) / 2)
    assertNextStepOf(new Bird(0, 0, 270, 10), -10, 0)
    assertNextStepOf(new Bird(0, 0, 315, 10), -sqrt(200) / 2, sqrt(200) / 2)
    assertNextStepOf(new Bird(0, 0, 360, 10), 0, 10)
    assertNextStepOf(new Bird(0, 0, 450, 10), 10, 0)
  }

  static assertNextStepOf(Bird bird, double x, double y) {
    bird.update()
    assertEquals("", bird.x, x, 0.01)
    assertEquals("", bird.y, y, 0.01)
  }
}

class MyPanel extends JPanel {

  List<Bird> birds

  MyPanel(List<Bird> birds) {
    this.birds = birds
    addMouseMotionListener(new MouseMotionAdapter() {
      @Override
      void mouseMoved(MouseEvent e) {
        birds[0].with { moveTowards(e.x, e.y) }
      }
    })
  }

  @Override
  protected void paintComponent(Graphics g) {
    def g2 = (Graphics2D) g
    birds[0].with {
//      direction += direction * 0.05 + 15
    }
    birds.each {
      it.update()
      g2.drawRect((int) it.x, (int) it.y, 2, 2)
    }
  }
}
