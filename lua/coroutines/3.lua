package.path = package.path .. ";../?.lua"
require("common")

local events = {}
function log(event)
  events[#events + 1] = event
end

-- Yield/resume from coroutine subfunction

function subfunction()
  log("subfunction started")
  coroutine.yield(42)
  log("subfunction finished")
end

local c = coroutine.create(function(n)
  log("coroutine started")
  subfunction()
  log("coroutine finished")
end)

log("main started")
_, n = coroutine.resume(c)
log("main received: " .. to_s(n))
_, n = coroutine.resume(c)
log("main received: " .. to_s(n))
log("main finished")


expect_to_be_equal(events, {
  [1] = "main started",
  [2] = "coroutine started",
  [3] = "subfunction started",
  [4] = "main received: 42",
  [5] = "subfunction finished",
  [6] = "coroutine finished",
  [7] = "main received: nil",
  [8] = "main finished",
})
