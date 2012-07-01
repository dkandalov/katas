require "rspec"

shared_examples_for "Connectivity" do

  it "should determine if two nodes are connected" do
    connectivity.connected(0, 1).should be_false
    connectivity.connect(0, 1)
    connectivity.connected(0, 1).should be_true
    connectivity.connected(1, 0).should be_true
  end

  it "should work with nodes connected twice both ways" do
    connectivity.connected(0, 1).should be_false
    connectivity.connect(0, 1)
    connectivity.connect(1, 0)
    connectivity.connected(0, 1).should be_true
  end

  it "should work for this example" do
    %w{3-4 4-9 8-0 2-3 5-6 2-9 5-9 7-3 4-8 5-6 0-2 6-1}.map(&:to_int_pair)
    .each {|pair| connectivity.connect(pair[0], pair[1])}

    (0..9).to_a.product((0..9).to_a).each{|pair|
      connectivity.connected(pair[0], pair[1]).should be_true
    }
  end

  class String
    def to_int_pair
      split("-").map(&:to_i)
    end
  end
end
