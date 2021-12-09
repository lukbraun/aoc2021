local open = io.open
local flatten = require('table.flatten')
local split = require('string.split')

local filename = "input"
local filenameTest = "input_test"

local function map(tbl, f)
    local t = {}
    for k,v in pairs(tbl) do
        t[k] = f(v)
    end
    return t
end

local function read_file(path)
    local file = open(path, "rb")
    if not file then return nil end
    local content = file:read "*a"
    file:close()
    local function splitStr (x)
        return split(x, "")
    end
    return map(split(content, "\n"), splitStr)
end

local function count(l)
    local counter = 0
    for _ in pairs(l) do
        counter = counter + 1
    end
    return counter
end

local function isSmallest(table, line, column)
    local res = true
    local lines = count(table)
    local columns = count(table[line])

    -- check right
    if column < columns then
        res = res and table[line][column] < table[line][column + 1]
    end
    -- check left
    if column > 1 then
        res = res and table[line][column] < table[line][column - 1]
    end
    -- check up
    if line > 1 then
        res = res and table[line][column] < table[line - 1][column]
    end
    -- check down
    if line < lines then
        res = res and table[line][column] < table[line + 1][column]
    end

    return res
end

local lookedUp = {}

local function countBasinSize (tab, startLine, startColumn)
    local maxLen = count(tab)
    local maxCol = count(tab[1])
    local function size(line, col, acc)
        local num = tonumber(tab[line][col])
        if num == 9 or lookedUp[line][col] == true then
            return acc
        end
        lookedUp[line][col] = true
        local counter = acc
        if line + 1 <= maxLen then
            counter  = size(line + 1, col, counter)
        end
        if line - 1 >= 1 then
            counter = size(line - 1, col, counter)
        end
        if col + 1 <= maxCol then
            counter = size(line, col + 1, counter)
        end
        if col - 1 >= 1 then
            counter = size(line, col - 1, counter)
        end
        return counter + 1
    end
    local res = size(startLine, startColumn, 0)
    return res
end

local function main(filename)
    local content = read_file(filename)
    local sum = 0
    local t = {}
    local basinSizes = {}
    for i = 1, count(content) do
        lookedUp[i] = {}
        for j = 1, count(content[i]) do
            lookedUp[i][j] = false
        end
    end
    for i = 1, count(content) do
        table.insert(t, {})
        for j = 1, count(content[i]) do
            table.insert(t[#t], isSmallest(content, i, j))
            if isSmallest(content, i, j) then
                sum = sum + content[i][j] + 1
                table.insert(basinSizes, countBasinSize(content, i, j))
            end
        end
    end
    table.sort(basinSizes, function (a, b)
        return a > b
    end)
    local res = basinSizes[1] * basinSizes[2] * basinSizes[3]

    print("sum: " .. sum)
    print("basins: " .. res)
end

main(filename)