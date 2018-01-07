function ends_in_three(n)
  return n % 10 == 3
end

function is_prime(n)
  for i = 2, n / 2 do
    if n % i == 0 then
      return false
    end
  end
  return true
end

function primes_ending_with_three(n, i)
  if i == nil then i = 3 end
  if n == 0 then return end
  if ends_in_three(i) and is_prime(i) then
    print(i)
    n = n - 1
  end
  primes_ending_with_three(n, i + 1)
end

function for_loop(a, b, f)
  while a <= b do
    f(a)
    a = a + 1
  end
end

function reduce(max, init, f)
  result = init
  i = 1
  while i <= max do
    result = f(result, i)
    i = i + 1
  end
  return result
end

function add(a, b)
  return a + b
end

function expect_to_be_equal(actual, expected)
  if actual != expected then
    error("Expected: " .. tostring(expected).. "\nbut was: " .. tostring(actual))
  end
end

print(is_prime(1))
print(is_prime(2))
print(is_prime(3))
print(is_prime(4))
print(is_prime(5))
print(is_prime(6))
print(is_prime(7))
print(is_prime(8))
print(is_prime(13))
print("------")
print(primes_ending_with_three(5))
print("------")
for_loop(1, 3, print)
print("------")
print(reduce(5, 0, add))
