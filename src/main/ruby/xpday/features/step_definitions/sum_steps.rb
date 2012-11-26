Given /^the first number is (\d+)$/ do |number|
  @first = number
end

When /^the second number is (\d+)$/ do |number|
  @second = number
end

require 'rspec/expectations'

Then /^the sum of these numbers is (\d+)$/ do |number|
  sum = string_sum(@first, @second)
  sum.should == number
end

