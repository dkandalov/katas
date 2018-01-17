package.path = package.path .. ";../?.lua"
require("common")
scheduler = require "scheduler"

------------------
-- Multitasking --
------------------
local actions = {}

function punch()
  for i = 1, 5 do
    print('punch ' .. i)
    actions[#actions + 1] = 'punch ' .. i
    scheduler.wait(1.0)
  end
end

function block()
  for i = 1, 3 do
    print('block ' .. i)
    actions[#actions + 1] = 'block ' .. i
    scheduler.wait(2.0)
  end
end

scheduler.schedule(0.0, coroutine.create(punch))
scheduler.schedule(0.0, coroutine.create(block))
scheduler.run()

expect_to_be_equal(actions, {
  [1] = "punch 1",
  [2] = "block 1",
  [3] = "punch 2",
  [4] = "block 2",
  [5] = "punch 3",
  [6] = "punch 4",
  [7] = "block 3",
  [8] = "punch 5"
})
