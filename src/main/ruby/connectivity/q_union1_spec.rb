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
  it "should determine if nodes are connected" do
    qunion = QUnion.new(10)

    qunion.connected(0, 1).should be_false
    qunion.connect(0, 1).should be_false
    qunion.connected(0, 1).should be_true
  end
end