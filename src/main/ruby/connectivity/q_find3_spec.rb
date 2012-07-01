require "rspec"
require_relative "connectivity_example_spec"

class QFind

end

describe QFind do
  it_behaves_like "Connectivity" do
    let(:connectivity) { QFind.new(10) }
  end
end
