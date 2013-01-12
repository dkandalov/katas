require "rspec"

describe "My behaviour" do

  def readLinesFrom(file_name)
    File.new(file_name).readlines
  end

  def filter_data(lines)
    lines[8...(lines.size - 2)]
  end

  def extract_data_columns(lines)
    lines.map { |line| line.strip.split(/\s+/) }.map { |line| [line[0], line[1], line[2]]}
  end

  def convert_data(lines)
    lines.map { |line| line.map { |s| s.to_i } }
  end

  def row_with_min_value_diff(rows)
    rows.min_by{ |row| (row[1] - row[2]).abs }
  end

  def find_day_with_min_temperature_spread(weather_file_name)
    row = row_with_min_value_diff(convert_data(extract_data_columns(filter_data(readLinesFrom(weather_file_name)))))
    row.nil? ? nil : row[0]
  end

  it "should do something" do
    weather_file_name = "/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat"

    readLinesFrom(weather_file_name).size.should == 40
    filter_data(readLinesFrom(weather_file_name)).size.should == 30

    extract_data_columns(filter_data(readLinesFrom(weather_file_name))).size.should == 30
    extract_data_columns(filter_data(readLinesFrom(weather_file_name)))[0].should == %w(1 88 59)
    extract_data_columns(filter_data(readLinesFrom(weather_file_name)))[29].should == %w(30 90 45)

    convert_data(extract_data_columns(filter_data(readLinesFrom(weather_file_name)))).size.should == 30
    convert_data(extract_data_columns(filter_data(readLinesFrom(weather_file_name))))[0].should == [1, 88, 59]
    convert_data(extract_data_columns(filter_data(readLinesFrom(weather_file_name))))[8].should == [9, 86, 32]
    convert_data(extract_data_columns(filter_data(readLinesFrom(weather_file_name))))[29].should == [30, 90, 45]

    row_with_min_value_diff(convert_data(extract_data_columns(filter_data(readLinesFrom(weather_file_name))))).should == [14, 61, 59]

    find_day_with_min_temperature_spread(weather_file_name).should == 14
  end
end