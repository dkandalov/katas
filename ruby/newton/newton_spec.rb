require 'rspec'

describe 'Newton method' do
  it 'should approximate square root of a number' do
    sqrt(1.0).should be_close_to 1.0
    sqrt(2.0).should be_close_to 1.41421
    sqrt(10.0).should be_close_to 3.16227
  end

  def sqrt(n, guess = 1.0, threshold = 0.00001)
    guess_is_good_enough = proc { (guess * guess - n).abs < threshold }
    improved_guess = proc { guess - ((guess * guess - n) / (2 * guess)) }

    return guess if guess_is_good_enough.call
    sqrt n, improved_guess.call, threshold
  end

  def be_close_to(n, threshold = 0.00001)
    be_within(threshold).of(n)
  end
end