package.path = package.path .. ";../?.lua"
require("common")

local queue = {}
function add(item)
  queue[#queue + 1] = item
end
function remove()
  if #queue == 0 then
    return nil
  end
  local result = queue[#queue]
  queue[#queue] = nil
  return result
end

local consumer = nil
local producer = coroutine.create(function()
  local i = 0
  while i < 2 do
    add(i)
    add(to_s(i) .. "_")
    print("added " .. i)
    coroutine.resume(consumer)
    print("resumed")
    i = i + 1
  end
end)
consumer = coroutine.create(function()
  while true do
    while #queue > 0 do
      print(remove())
    end
    coroutine.resume(producer)
    print("here")
    if #queue == 0 then
      print("done")
      return
    end
  end
end)

coroutine.resume(producer)
