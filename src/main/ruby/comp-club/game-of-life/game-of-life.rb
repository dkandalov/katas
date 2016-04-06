#!/usr/bin/ruby

class LiveCell
  attr_reader :x, :y

  def initialize(x,y)
    @x = x
    @y = y
  end

  def ==(other)
    @x == other.x && @y == other.y
  end
end

class Grid
  def initialize(live_cells)
    @live_cells = live_cells
    @width = (live_cells.map(&:x).max || 0) + 2
    @height = (live_cells.map(&:y).max || 0) + 2
  end

  def tick
    new_live_cells =
        @width.times.flat_map do |x|
          @height.times.map do |y|
            if @live_cells.include?(LiveCell.new(x,y))
              if [2,3].include? live_neighbour_count(x,y)
                LiveCell.new(x,y)
              end
            else
              if live_neighbour_count(x,y) == 3
                LiveCell.new(x,y)
              end
            end
          end
        end.compact

    Grid.new(new_live_cells)
  end

  def to_s
    @height.times.map do |y|
      @width.times.map do |x|
        if @live_cells.include?(LiveCell.new(x,y))
          live_neighbour_count(x,y)
        else
          ' '
        end
      end.join('')
    end.join("\n")
  end

  private

  def live_neighbour_count(x, y)
    (x-1..x+1).flat_map do |xi|
      (y-1..y+1).map do |yi|
        (x != xi || y != yi) && @live_cells.include?(LiveCell.new(xi,yi))
      end
    end.count(true)
  end
end

def cells(*tuples)
  tuples.map do |it|
    LiveCell.new(it[0], it[1])
  end
end

block = cells(
    [1, 1], [2, 1],
    [1, 2], [2, 2]
)
beehive = cells(
    [2, 1], [3, 1],
    [1, 2], [4, 2],
    [2, 3], [3, 3],
)
blinker = cells([1, 1], [1, 2], [1, 3])
toad = cells(
    [2, 1], [3, 1], [4, 1],
    [1, 2], [2, 2], [3, 2],
)
glider = cells([2, 1], [3, 2], [1, 3], [2, 3], [3, 3])
random_cells = (1..400).map do
  LiveCell.new(rand(40), rand(40))
end.uniq

g = Grid.new(toad)
100.times do
  system('clear')
  puts g.to_s
  g = g.tick
  sleep(2)
end