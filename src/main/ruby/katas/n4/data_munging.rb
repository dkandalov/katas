require "test/unit"

#arcade
class DataMunging0 < Test::Unit::TestCase
  def test_aaa
    day_with_min_spread = File.read("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat").split("\n")
      .keep_if { |line| line.match(/^\s*\d+.*$/) }
      .map { |line| line.gsub(/\*/, "").split }
      .map { |cols| {id: cols[0], diff: (cols[2].to_i - cols[1].to_i).abs} }
      .min_by { |val| val[:diff] }[:id]
    assert day_with_min_spread == "14"
  end
end