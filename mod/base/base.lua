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
    newWindow (n,t)
end
function LuaWindow:addText (x,y,text)
    newText ((self.lwName),x,y,text,"text")
end
function LuaWindow:addTTF (x,y,text)
    newText ((self.lwName),x,y,text,"ttfbox")
end
function LuaWindow:addTitle (x,y,text)
    newText ((self.lwName),x,y,text,"title")
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
function LuaWindow:newButton (x,y,text,action,args)
    newText ((self.lwName),x,y,text,"textbox")
    newLink ((self.lwName),x,y,text,action,args)
end
function LuaWindow:addMenu (mName,x,y)
    newMenu ((self.lwName),mName,x,y)
end
function LuaWindow:addMenuBit (mName,mbType,mbArgs)
    newMenuBit ((self.lwName),mName,mbType,mbArgs)
end
function LuaWindow:addWorld (zx,zy,sx,sy,dp)
    newWorld ((self.lwName),zx,zy,sx,sy,dp)
end
function LuaWindow:addFPS ()
    newDynObj ((self.lwName),"fps")
end
-- this runs once at the beginning
function initLua ()
    -- basic UI elements can be in any order
    local menu1 = LuaWindow:new ()
    menu1:init("menu1","menu")
    menu1:setBackground ("dat/tex/texture1.png")
    menu1:addText (-4.0, 4.0, "A Bridge Far Away")
    menu1:newButton (-4.0, 2.0, "Create World", "link", "menu2")
    menu1:newButton (-4.0, -2.0, "Escape", "action", "exit")
    menu1:addTitle (-6.0,0.5,"asdfghj")
    menu1:addTTF (-6.0,-0.5,"asdfghj")

    local menu2 = LuaWindow:new ()
    menu2:init("menu2","menu")
    menu2:setBackground ("dat/tex/texture1.png")
    menu2:addMenu ("submenu1",-4.0, 2.0)
    menu2:addMenuBit ("submenu1","text","World Parameters")
    menu2:addMenuBit ("submenu1","slider","nConts:1-10,4")
    menu2:addMenuBit ("submenu1","slider","Seed:1-1000,10")
    menu2:newButton (-4.0, -2.5, "Create World", "link", "game1")
    menu2:newButton (-4.0, -4.0, "Back", "action", "back")

    local game1 = LuaWindow:new ()
    game1:init("game1","game")
    game1:setBackground ("dat/tex/black.png")
    game1:addTextBox (-4.0, 4.0, "blop blop")
    game1:addWorld (8,8,16,8,"dat/tex/world/")

    menu1:switchToWindow ()
    loadModule ("mod/base/game.lua")

    return 0
end
