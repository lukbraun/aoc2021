local open = io.open
local flatten = require('table.flatten')
local split = require('string.split')

local function map(tbl, f)
    local t = {}
    for k,v in pairs(tbl) do
        t[k] = f(v)
    end
    return t
end

local function filter(tbl, p)
    local t = {}
    for k, v in pairs(tbl) do
        if p(v) then
            table.insert(t, tbl[k])
        end
    end
    return t
end

local function read_file(path)
    local file = open(path, "rb")
    if not file then return nil end
    local content = file:read "*a"
    file:close()
    return split(content, "\n")
end

local function get_second(x)
    return x[2]
end

local function get_notes (list, pos) 
    local function split_by_pipe (x)
        return split(x, "|")
    end

    local function split_by_whitespace (x)
        return split(x, " ")
    end

    return map(map(map(list, split_by_pipe), pos), split_by_whitespace)
end

local function get_input(filename)
    return read_file(filename)
end

local function is1(x)
    return string.len(x) == 2
end
local function is4(x)
    return string.len(x) == 4
end
local function is7(x)
    return string.len(x) == 3
end
local function is8(x)
    return string.len(x) == 7
end

local function only_2_3_4_7 (list)
    local function is_unique(x)
        return is1(x) or is7(x) or is4(x) or is8(x)
    end

    return filter(flatten(list, 0), is_unique)
end

local function count(l)
    local counter = 0
    for _ in pairs(l) do
        counter = counter + 1
    end
    return counter
end

local function main_1(content)
    local t = only_2_3_4_7(get_notes(content, get_second))
    print("number of unique digits: " .. count(t))
end

local function intersection(str1, str2)
    local res = ""
    for s1 in str1:gmatch"." do
        for s2 in str2:gmatch"." do
            if s1 == s2 then
                res = res .. s1
            end
        end
    end
    return res
end

local function stringDoesNotContainChar(str, char)
    for s1 in str:gmatch"." do
        if s1 == char then
            return false
        end
    end
    return true
end

local function difference(str1, str2)
    local res = ""
    for s1 in str1:gmatch"." do
        if stringDoesNotContainChar(str2, s1) then
            res = res .. s1
        end
    end
    for s2 in str2:gmatch"." do
        if stringDoesNotContainChar(str1, s2) then
            res = res .. s2
        end
    end
    return res
end

local function sortString(str)
    local tab = split(str, "")
    table.sort(tab)
    local res = ""
    for key2, value2 in pairs(tab) do
        res = res .. value2
    end
    return res
end

local function decode(input)
    local decoded = {
        ["0"] = "",
        ["1"] = "",
        ["2"] = "",
        ["3"] = "",
        ["4"] = "",
        ["5"] = "",
        ["6"] = "",
        ["7"] = "",
        ["8"] = "",
        ["9"] = ""
    }

    local function filterOut(str)
        for key, value in pairs(decoded) do
            if value == str then
                return false
            end
        end
        return true
    end

    local function len6(x)
        return string.len(x) == 6
    end

    local function is3(x)
        if string.len(x) == 5 then
            local diff = difference(decoded["1"], x)
            return string.len(diff) == 3
        end
        return false
    end

    local function is9(x)
        if string.len(x) == 6 then
            local diff1 = difference(x, decoded["4"])
            local diff2 = difference(decoded["7"], decoded["1"])
            local totalDiff = difference(diff1, diff2)
            return string.len(totalDiff) == 1
        end
        return false
    end

    local function is6(x)
        if string.len(x) == 6 then
            local diff1 = difference(x, decoded["8"])
            return string.len(intersection(diff1, decoded["7"])) == 1
        end
        return false
    end

    local function is2(x)
        if string.len(x) == 5 then
            return string.len(difference(x, decoded["6"])) == 3
        end
        return false
    end

    decoded["1"] = filter(input, is1)[1]
    decoded["4"] = filter(input, is4)[1]
    decoded["7"] = filter(input, is7)[1]
    decoded["8"] = filter(input, is8)[1]
    decoded["3"] = filter(input, is3)[1]
    decoded["9"] = filter(input, is9)[1]
    decoded["6"] = filter(input, is6)[1]
    decoded["0"] = filter(filter(input, len6), filterOut)[1]
    decoded["2"] = filter(filter(input, filterOut), is2)[1]
    decoded["5"] = filter(input, filterOut)[1]

    for key, value in pairs(decoded) do
        decoded[key] = sortString(value)
    end

    local result = {}
    for key, value in pairs(decoded) do
        result[value] = key
    end

    return result
end

local function main_2(content)
    local firstPart = get_notes(content, function (x)
        return x[1]
    end)
    local secondPart = get_notes(content, get_second)
    local sum = 0
    for i = 1, count(content) do
        local decoded = decode(firstPart[i])
        local res = {}
        for key, value in pairs(secondPart[i]) do
            if key ~= 1 then
                table.insert(res, sortString(value))
            end
        end
        local exp = 3
        for j = 1, count(res) do
            local num = decoded[res[j]]
            print("res: " .. res[j])
            sum = sum + (num * 10 ^ exp)
            exp = exp - 1
        end
    end
    print("total sum is " .. sum)
end

local fileContent = get_input("input")

main_1(fileContent)
main_2(fileContent)