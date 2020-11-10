-- base mod defines some basic gameplay
-- this data type represents a classlike
-- interface to the haskell objects
LuaWindow = {lwName = "NULLname"}
function LuaWindow:new (o)
    o = o or {}
    self.__index = self
    setmetatable (o, self)
    self.lwName = "NULL"
    return o
end
function LuaWindow:init (n,t)
    self.lwName = n
    newWindow (n,t,"menu")
end
function LuaWindow:addText (x,y,text)
    newText ((self.lwName),x,y,text,"text")
end
function LuaWindow:addTextBox (x,y,text)
    newText ((self.lwName),x,y,text,"textbox")
end
function LuaWindow:setBackground (fp)
    setBackground (self.lwName,fp)
end
function LuaWindow:switchToWindow ()
    switchWindow (self.lwName)
end
-- this runs once at the beginning
function initLua ()
    local menu1 = LuaWindow:new ()
    local menu2 = LuaWindow:new ()
    menu1:init("menu1","menu")
    menu2:init("menu2","menu")
    menu1:setBackground ("dat/tex/texture1.png")
    menu1:addText (-4.0, 4.0, "A Bridge Far Away")
    menu1:addTextBox (-4.0, 2.0, "Create World")
    menu1:addTextBox (-4.0, -2.0, "Escape")
    menu1:switchToWindow ()
    return 0
end
