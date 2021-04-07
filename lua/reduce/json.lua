-- the hard part - making a lame json encoder
local function indent(str, size)
  local prefix = ''
  for _ = 1,size do
    prefix = prefix .. ' '
  end
  local result = {}
  local i = 0
  for row in string.gmatch(str, "([^\n]+)") do
    i = i + 1
    result[i] = prefix .. row
  end
  return table.concat(result, "\n")
end

local function encode(object, depth)
  depth = depth or 0
  local ty = type(object)

  if ty == 'table' then
    -- NOTE: now there is some ambiguty with tables regarding json
    --       you can't tell an empty map apart from an empty array,
    --       you can misinterpret a table with integral keys as an array
    --       for this particular purpose we know that:
    --         1. there are no numeric keys in the tables
    --         2. we have no empty tables
    --         3. if it has a numeric key, it's an array.
    if object[1] then
      -- it has some kind of numeric key (starting from 1)
      -- therefore we assume it's an array
      local result = {}
      local i = 0
      for _,value in ipairs(object) do
        i = i + 1
        result[i] = encode(value, depth + 1)
      end

      return "[\n" .. indent(table.concat(result, ",\n"), 2) .. "\n]"
    else
      -- it's a map/hash/table/object
      local result = {}
      local i = 0
      for key,value in pairs(object) do
        -- json only supports strings as keys
        -- though it's already known that this script uses only strings
        -- for keys, it's matter of covering all the cases here.
        local json_key = encode(tostring(key), depth + 1)
        local json_value = encode(value, depth + 1)
        i = i + 1
        result[i] = json_key .. ": " .. json_value
      end
      return "{\n" .. indent(table.concat(result, ",\n"), 2) .. "\n}"
    end
  elseif ty == 'string' then
    -- NOTE: doing the laziest possible string encoding here
    return '"' .. tostring(object) .. '"'
  elseif ty == 'number' then
    return tostring(object)
  elseif ty == 'boolean' then
    return tostring(object)
  elseif ty == nil then
    return 'null'
  else
    error("unexpected type " .. ty)
  end
end

return { encode = encode }
