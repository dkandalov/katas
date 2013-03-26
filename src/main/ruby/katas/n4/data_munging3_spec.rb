require "rspec"

describe "Data munging" do
  diff = proc{ |line| (line[1] - line[2]).abs }

  it "should find day with min temperature spread" do
    lines = File.new("#{ENV['HOME']}/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat").readlines
    lines = lines[8..-3]

    lines = lines.map{ |line| line.split(/\s+/) }.map { |line|
      [line[1], line[2].to_i, line[3].to_i]
    }
    day = lines.min_by(&diff)

    lines.size.should == 30
    day.should == ["14", 61, 59]
  end

  it "should find team with goal difference" do
    lines = File.new("#{ENV['HOME']}/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat").readlines
    lines = lines[5..-2]
    lines = lines.map{ |line|
      if line.include?("--------")
        nil
      else
        line = line.split(/\s+/)
        [line[2], line[7].to_i, line[9].to_i]
      end
    }.find_all{|line| not line.nil?}
    day = lines.min_by(&diff)

    day.should == ["Aston_Villa", 46, 47]
    lines.size.should == 20
  end
end