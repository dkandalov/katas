require "rspec"

describe "Data munging which" do

  def readLinesFrom(file_name)
    File.new(file_name).readlines
  end

  def filter_data(lines, head_drop, tail_drop)
    lines[head_drop...(lines.size - tail_drop)].find_all {|line| not line.include?("----")}
  end

  def extract_data_columns(lines, key_column, value1_column, value2_column)
    lines.map { |line| line.strip.split(/\s+/) }.map do |line|
      [line[key_column], line[value1_column].to_i, line[value2_column].to_i]
    end
  end

  def row_with_min_value_diff(rows)
    rows.min_by{ |row| (row[1] - row[2]).abs }
  end

  def find_day_with_min_temperature_spread(weather_file_name)
    row_with_min_value_diff(extract_data_columns(filter_data(readLinesFrom(weather_file_name), 8, 2), 0, 1, 2))[0]
  end


  def find_team_with_min_goal_diff(football_file)
    row_with_min_value_diff(extract_data_columns(filter_data(readLinesFrom(football_file), 5, 1), 1, 6, 8))[0]
  end

  it "should find day with minimum temperature spread" do
    weather_file_name = "#{ENV['HOME']}/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat"

    readLinesFrom(weather_file_name).size.should == 40
    filter_data(readLinesFrom(weather_file_name), 8, 2).size.should == 30

    extract_data_columns(filter_data(readLinesFrom(weather_file_name), 8, 2), 0, 1, 2).size.should == 30
    extract_data_columns(filter_data(readLinesFrom(weather_file_name), 8, 2), 0, 1, 2)[0].should == ["1", 88, 59]
    extract_data_columns(filter_data(readLinesFrom(weather_file_name), 8, 2), 0, 1, 2)[8].should == ["9", 86, 32]
    extract_data_columns(filter_data(readLinesFrom(weather_file_name), 8, 2), 0, 1, 2)[29].should == ["30", 90, 45]

    row_with_min_value_diff(extract_data_columns(filter_data(readLinesFrom(weather_file_name), 8, 2), 0, 1, 2)).should == ["14", 61, 59]

    find_day_with_min_temperature_spread(weather_file_name).should == "14"
  end

  it "should find team with minimum goal difference" do
    football_file = "#{ENV['HOME']}//IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat"
    find_team_with_min_goal_diff(football_file).should == "Aston_Villa"
  end
end