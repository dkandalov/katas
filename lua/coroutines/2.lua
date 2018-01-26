package.path = package.path .. ";../?.lua"
require("common")

local events = {}
function log(event)
  events[#events + 1] = event
end

-- Send/receive values from coroutine

local c = coroutine.create(function(n)
  log("c received: " .. n)
  n = coroutine.yield(2)
  log("c received: " .. n)
end)

log("main started")
_, n = coroutine.resume(c, 1)
log("main received: " .. to_s(n))
_, n = coroutine.resume(c, 1)
log("main received: " .. to_s(n))
log("main finished")

expect_to_be_equal(events, {
  [1] = "main started",
  [2] = "c received: 1",
  [3] = "main received: 2",
  [4] = "c received: 1",
  [5] = "main received: nil",
  [6] = "main finished",
})
