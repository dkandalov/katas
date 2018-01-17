scheduler = require 'scheduler'

------------------
-- Multitasking --
------------------
function punch()
  for i = 1, 5 do
    print('punch ' .. i)
    scheduler.wait(1.0)
  end
end

function block()
  for i = 1, 3 do
    print('block ' .. i)
    scheduler.wait(2.0)
  end
end

scheduler.schedule(0.0, coroutine.create(punch))
scheduler.schedule(0.0, coroutine.create(block))
scheduler.run()
