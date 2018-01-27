package.path = package.path .. ";../?.lua"
require("common")

local events = {}
function log(event)
  events[#events + 1] = event
end

-- Yield by resuming another coroutine

local c2 = coroutine.create(function()
  log("c2 started")
  coroutine.yield()
  log("c2 finished")
end)
local c1 = coroutine.create(function()
  log("c1 started")
  coroutine.resume(c2)
  log("c1 finished")
end)

coroutine.resume(c1)
coroutine.resume(c2)

print(to_s(events))
