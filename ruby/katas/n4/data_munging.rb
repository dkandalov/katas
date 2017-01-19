require "test/unit"

#arcade
class DataMunging0 < Test::Unit::TestCase
  def test_finds_day_with_min_temperature_spread
    day = find_min("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat", 0, 2, 1)
    assert_equal "14", day
  end

  def test_finds_team_with_min_goals_diff
    team = find_min("/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat", 1, 8, 6)
    assert_equal "Aston_Villa", team
  end

  def find_min filename, id_col, max_col, min_col
    File.read(filename).split("\n")
     .keep_if { |line| line.match(/^\s*\d+.*$/) }
     .map { |line| line.gsub(/\*/, "").split }
     .map { |cols| {id: cols[id_col], diff: (cols[max_col].to_i - cols[min_col].to_i).abs} }
     .min_by { |val| val[:diff] }[:id]
  end
end