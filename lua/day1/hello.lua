require "common"

function ends_in_three(n)
  return n % 10 == 3
end

expect_to_be_equal(ends_in_three(1), false)
expect_to_be_equal(ends_in_three(3), true)
expect_to_be_equal(ends_in_three(23), true)

function is_prime(n)
  for i = 2, n / 2 do
    if n % i == 0 then
      return false
    end
  end
  return true
end

expect_to_be_equal(is_prime(1), true)
expect_to_be_equal(is_prime(2), true)
expect_to_be_equal(is_prime(3), true)
expect_to_be_equal(is_prime(4), false)
expect_to_be_equal(is_prime(5), true)
expect_to_be_equal(is_prime(6), false)
expect_to_be_equal(is_prime(7), true)
expect_to_be_equal(is_prime(8), false)
expect_to_be_equal(is_prime(13), true)


function primes_ending_with_three(n, i, result)
  if i == nil then i = 3 end
  if result == nil then result = {} end
  if n == 0 then return result end
  if ends_in_three(i) and is_prime(i) then
    result[#result + 1] = i
    n = n - 1
  end
  return primes_ending_with_three(n, i + 1, result)
end

expect_to_be_equal(primes_ending_with_three(5), {
  [1] = 3,
  [2] = 13,
  [3] = 23,
  [4] = 43,
  [5] = 53
})


function for_loop(a, b, f)
  while a <= b do
    f(a)
    a = a + 1
  end
end
print("=== testing for_loop() ===")
for_loop(1, 3, print)
for_loop(-1, 1, print)

function add(a, b)
  return a + b
end

function reduce(max, init, f)
  local result = init
  i = 1
  while i <= max do
    result = f(result, i)
    i = i + 1
  end
  return result
end

expect_to_be_equal(reduce(0, 0, add), 0)
expect_to_be_equal(reduce(1, 0, add), 1)
expect_to_be_equal(reduce(2, 0, add), 1 + 2)
expect_to_be_equal(reduce(5, 0, add), 1 + 2 + 3 + 4 + 5)

function factorial(n)
  function mult(a, b)
    return a * b
  end
  return reduce(n, 1, mult)
end

expect_to_be_equal(factorial(0), 1)
expect_to_be_equal(factorial(1), 1)
expect_to_be_equal(factorial(2), 2)
expect_to_be_equal(factorial(3), 6)
expect_to_be_equal(factorial(4), 24)
