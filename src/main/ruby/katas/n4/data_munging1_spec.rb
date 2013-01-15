require "rspec"

describe "Data munging" do
  it "should find day with min temperature spread" do
    lines = File.new("#{ENV['HOME']}/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat").readlines
    lines = lines[8..(lines.size-3)]
    lines = lines.map {|line| line.strip.split(/\s+/)}.map{|line| [line[0], line[1].to_i, line[2].to_i] }
    lines.min_by{|line| (line[1] - line[2]).abs }[0].should == "14"
  end

  it "should find team with min goal difference" do
    lines = File.new("#{ENV['HOME']}/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat").readlines
    lines = lines[5..(lines.size-2)].find_all {|line| not line.include?("-----")}
    lines = lines.map {|line| line.strip.split(/\s+/)}.map{|line| [line[1], line[6].to_i, line[8].to_i] }
    lines.min_by{|line| (line[1] - line[2]).abs }[0].should == "Aston_Villa"
  end
end