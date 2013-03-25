require "rspec"

describe "Data munging" do
  it "should find day with min temperature spread" do
    lines = File.new("#{ENV['HOME']}/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat").readlines
    lines = lines[8..-3]
    day = lines.map{|line| line.split(/\s+/)}
                 .map {|line| [line[1], line[2].to_i, line[3].to_i]}
                 .map {|line| [line[0], (line[1] - line[2]).abs]}
                 .min_by{|line| line[1]}

    lines.size.should == 30
    day.should == ["14", 2]
  end

  it "should find team with min goal difference" do
    lines = File.new("#{ENV['HOME']}/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat").readlines
    lines.size.should == 27
    lines = lines[5..-2].find_all{|line| not line.include?("----")}
    team = lines.map{|line| line.split(/\s+/) }
                .map { |line| [line[2], line[7].to_i, line[9].to_i]}
                .map { |line| [line[0], (line[1] - line[2]).abs]}
                .min_by{|line| line[1]}
    team.should == ["Aston_Villa", 1]
  end
end