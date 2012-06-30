require "rspec"

class QUnion
  def initialize size
    @refs = (0...size).to_a
  end

  def connected node1, node2
    false
  end

  def connect node1, node2

  end
end

describe QUnion do
  it "should determine if two nodes are connected" do
    qunion = QUnion.new(10)

    qunion.connected(0, 1).should be_false
    qunion.connect(0, 1)
    qunion.connected(0, 1).should be_true
    qunion.connected(1, 0).should be_true
  end

  it "should work with nodes connected twice both ways" do
    qunion = QUnion.new(10)

    qunion.connect(0, 1)
    qunion.connect(1, 0)
    qunion.connected(0, 1).should be_true
  end

  it "should work on this example" do
    qunion = QUnion.new(10)

    %w{3-4 4-9 8-0 2-3 5-6 2-9 5-9 7-3 4-8 5-6 0-2 6-1}.map{|s| to_pair(s)}
      .each {|pair| qunion.connect(pair[0], pair[1])}

    (0..9).to_a.product((0..9).to_a).each{|pair|
      qunion.connected(pair[0], pair[1]).should eq(true)
    }
  end

  private

  def to_pair s
    s.split("-").map(&:to_i)
  end

end