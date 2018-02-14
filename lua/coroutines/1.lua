package.path = package.path .. ";../?.lua"
require("common")

local events = {}
function log(event)
  events[#events + 1] = event
end

-- Yield coroutine execution

local c = coroutine.create(function()
  log(2)
  coroutine.yield()
  log(4)
end)

log(1)
coroutine.resume(c)
log(3)
coroutine.resume(c)
log(5)

expect_to_be_equal(events, {
  [1] = 1,
  [2] = 2,
  [3] = 3,
  [4] = 4,
  [5] = 5,
})
