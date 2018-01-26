package.path = package.path .. ";../?.lua"
require("common")

function cc(depth)
  return coroutine.create(function()
    if depth > 0 then
      local c = cc(depth - 1)
      a(c)
      b(c)
    end
    print("started (depth: " .. depth .. ")")
    print("sending: 1")
    coroutine.yield(1)
    print("sending: 2")
    coroutine.yield(2)
    work("w")
    print("finished")
  end)
end

function work(id)
  print("work started" .. id)
  print(debug.traceback())
  coroutine.yield("")
  sub_work("sw")
  print("work finished " .. id)
end

function sub_work(id)
  print("sub_work started " .. id)
  print(debug.traceback())
  coroutine.yield("")
  print("sub_work finished " .. id)
end

local c = cc(1)

function a(c)
  _, r = coroutine.resume(c)
  print("received: " .. to_s(r))
  _, r = coroutine.resume(c)
  print("received: " .. to_s(r))
end
function b(c)
  print(debug.traceback())
  _, r = coroutine.resume(c)
  print("received: " .. to_s(r))
  _, r = coroutine.resume(c)
  print("received: " .. to_s(r))
  _, r = coroutine.resume(c)
  print("received: " .. to_s(r))
end

a(c)
b(c)
