package.path = package.path .. ";../?.lua"
require("common")

local _private = {}

function strict_read(table, key)
  if _private[key] then
    return _private[key]
  else
    error("Invalid key: " .. key)
  end
end

function strict_write(table, key, value)
  if _private[key] then
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

treasure.gold = 50
expect_to_be_equal(treasure.gold, 50)

expect_error("Invalid key: silver", function()
  print(treasure.silver)
end)

expect_error("Duplicate key: gold", function()
  treasure.gold = 100
end)
