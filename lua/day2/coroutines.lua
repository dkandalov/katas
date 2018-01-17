package.path = package.path .. ";../?.lua"
require("common")

------------------------------
-- Single Tasks: Generators --
------------------------------
function fibonacci()
  local m = 1
  local n = 1

  while true do
    coroutine.yield(m)
    m, n = n, m + n
  end
end

local f = coroutine.create(fibonacci)
expect_to_be_equal(type(f), "thread")
expect_to_be_equal(coroutine.status(f), "suspended")

succedded, value = coroutine.resume(f)
expect_to_be_equal(succedded, true)
expect_to_be_equal(value, 1)
succedded, value = coroutine.resume(f)
expect_to_be_equal(value, 1)
succedded, value = coroutine.resume(f)
expect_to_be_equal(value, 2)
succedded, value = coroutine.resume(f)
expect_to_be_equal(value, 3)
succedded, value = coroutine.resume(f)
expect_to_be_equal(value, 5)
