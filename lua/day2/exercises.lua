package.path = package.path .. ";../?.lua"
require("common")

----------
-- Easy --
----------
function concatenate(a1, a2)
  local result = {}
  for i = 1, #a1 do result[#result + 1] = a1[i] end
  for i = 1, #a2 do result[#result + 1] = a2[i] end
  return result
end

expect_to_be_equal(concatenate({}, {}), {})
expect_to_be_equal(concatenate({1}, {}), {1})
expect_to_be_equal(concatenate({}, {2}), {2})
expect_to_be_equal(concatenate({1}, {2, 3}), {1, 2, 3})
expect_to_be_equal(concatenate({1, 2, 3}, {4, 5, 6}), {1, 2, 3, 4, 5, 6})


local _private = {}
function strict_read(table, key)
  if _private[key] then
    return _private[key]
  else
    error("Invalid key: " .. key)
  end
end
function strict_write(table, key, value)
  if _private[key] and value ~= nil then
    error("Duplicate key: " .. key)
  else
    _private[key] = value
  end
end
local mt = {
  __index = strict_read,
  __newindex = strict_write
}
treasure = {}
setmetatable(treasure, mt)

treasure.gold = 100
expect_to_be_equal(treasure.gold, 100)
treasure.gold = nil
expect_error("Invalid key: gold", function()
  print(treasure.gold)
end)


------------
-- Medium --
------------
-- http://lua.2524044.n2.nabble.com/default-metatable-td7678773.html
-- https://stackoverflow.com/questions/10778812/how-do-i-add-a-method-to-the-table-type
local mt = {
  __add = concatenate
}
setmetatable(_G, {
  __newindex =
  function(table, key, value)
    rawset(table, key, value)
    setmetatable(table[key], mt)
  end
})

a1 = {1, 2}
a2 = {3}
expect_to_be_equal(a1 + a2, {1, 2, 3})

setmetatable(_G, nil) -- disable because the above code is hacky


local Queue = {
  items = {},
  i = 1
}
Queue.__index = Queue

function Queue.new()
  return setmetatable({}, Queue)
end

function Queue:add(item)
  self.items[#self.items + 1] = item
  return item
end

function Queue:remove()
  if #self.items == 0 then
    return nil
  end
  local result = self.items[self.i]
  self.items[self.i] = nil
  self.i = self.i + 1
  return result
end

q = Queue.new()
expect_to_be_equal(q:remove(), nil)
expect_to_be_equal(q:add(1), 1)
expect_to_be_equal(q:add(2), 2)
expect_to_be_equal(q:remove(), 1)
expect_to_be_equal(q:remove(), 2)
expect_to_be_equal(q:remove(), nil)


----------
-- Hard --
----------
function retry(count, body)
  local error_message = ""
  while error_message ~= nil do
    if count < 0 then
      return error("Reached retry limit")
    end
    error_message = coroutine.wrap(body)()
    count = count - 1
  end
end

local f = function()
  local r = math.random()
  print(r)
  if r > 0.2 then
    print("Failed")
    coroutine.yield('Something bad happened')
  end
  print('Succeeded')
end

expect_error("Reached retry limit", function()
  math.randomseed(1234)
  retry(5, f)
end)
