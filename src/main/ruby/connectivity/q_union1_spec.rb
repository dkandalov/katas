require "rspec"
require_relative "connectivity_example_spec"

class QUnion
  def initialize size
    @refs = (0...size).to_a
  end

  def connected node1, node2
    root_of(node1) == root_of(node2)
  end

  def connect node1, node2
      @refs[root_of(node1)] = root_of(node2)
  end

  private

  def root_of node
    @refs[node] == node ? node : root_of(@refs[node])
  end
end

describe QUnion do
  it_behaves_like "Connectivity" do
    let(:connectivity) { QUnion.new(10) }
  end
end
