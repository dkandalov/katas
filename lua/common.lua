function to_s(o)
  if type(o) == "table" then
    local s = ""
    for k,v in pairs(o) do
      s = s .. tostring(k) .. ": " .. tostring(v) .. "\n"
    end
    return s
  else
    return tostring(o)
  end
end

-- Copied from https://stackoverflow.com/questions/20325332/how-to-check-if-two-tablesobjects-have-the-same-value-in-lua
function equals(o1, o2, ignore_mt)
    if o1 == o2 then return true end
    local o1Type = type(o1)
    local o2Type = type(o2)
    if o1Type ~= o2Type then return false end
    if o1Type ~= 'table' then return false end

    if not ignore_mt then
        local mt1 = getmetatable(o1)
        if mt1 and mt1.__eq then
            --compare using built in method
            return o1 == o2
        end
    end

    local keySet = {}

    for key1, value1 in pairs(o1) do
        local value2 = o2[key1]
        if value2 == nil or equals(value1, value2, ignore_mt) == false then
            return false
        end
        keySet[key1] = true
    end

    for key2, _ in pairs(o2) do
        if not keySet[key2] then return false end
    end
    return true
end

function expect_to_be_equal(actual, expected)
  if not equals(actual, expected) then
    error("\nExpected: " .. to_s(expected).. "\nbut was: " .. to_s(actual))
  end
end

function expect_to_contain_equal(actual, expected)
  if not string.find(actual, expected) then
    error("\nExpected: '" .. to_s(actual).. "'\nto contain: '" .. to_s(expected) .. "'")
  end
end

function expect_error(expected, f)
  local actual = nil
  function error_handler(err)
    actual = err
  end
  xpcall(f, error_handler)
  expect_to_contain_equal(actual, expected)
end
