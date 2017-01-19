require "rspec"
require_relative "connectivity_example_spec"

class QFind
  def initialize size
    @refs = (0...size).to_a
  end

  def connected node1, node2
    @refs[node1] == @refs[node2]
  end

  def connect node1, node2
    old_ref = @refs[node1]
    (0...@refs.size).each { |i| @refs[i] = @refs[node2] if @refs[i] == old_ref }
  end
end

describe QFind do
  it_behaves_like "Connectivity" do
    let(:connectivity) { QFind.new(10) }
  end
end
