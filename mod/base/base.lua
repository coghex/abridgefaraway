-- base mod defines some basic gameplay
function getScreenSize ()
    return {2560, 1440}
end
function fontAtlas ()
    return "dat/tex/alph.png"
end
function textboxTexture ()
    return "dat/tex/box/"
end
function textureDirectory ()
    return "dat/tex/base"
end
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
function LuaWindow:init (n,t,background)
    self.lwName = n
    newWindow (n, t, background)
end
function LuaWindow:addText (x,y,text)
    newText ((self.lwName),x,y,text)
end
function LuaWindow:addTextBox (x,y,text)
    newButton ((self.lwName),x,y,text)
end
function LuaWindow:addButton (x,y,text,lw)
    newButtonAction ((self.lwName),x,y,text,"link",(lw.lwName))
end
function LuaWindow:addButtonAction (x,y,text,action)
    newButtonAction ((self.lwName),x,y,text,"action",action)
end

-- this runs once at the beginning
function initLua ()
    local menu1 = LuaWindow:new ()
    local menu2 = LuaWindow:new ()
    local game = LuaWindow:new ()
    menu1:init("menu1","menu","dat/tex/texture1.png")
    menu2:init("menu2","menu","dat/tex/texture1.png")
    game:init("game","game","dat/tex/black.png")
    menu1:addText (-4.0, 4.0, "A Bridge Far Away")
    menu1:addButton (-4.0, 2.0, "Create World", menu2)
    menu1:addTextBox (-4.0, 0.0, "Load World")
    menu1:addButtonAction (-4.0, -2.0, "Escape", "quit")
    menu2:addButton (-4.0, -2.0, "Create World", game)
    newMenu ("menu2", "createParamsMenu", -4.0, 2.0)
    --local paramsMenu = menu2:addMenu ("createParamsMenu", -4.0, 2.0,)
    newMenuElement ("createParamsMenu", "text", "World Parameters")
    --newMenuElement ("createParamsMenu", "slider", 10, 100, "width:"
    --newMenuElement ("createParamsMenu", "slider", 10, 100, "height:"
    --newButtonAction ("menu2", -4.0, -4.0, "back", "link", "menu1")
    menu2:addButton (-4.0, -4.0, "back", menu1)
    newWorld ( "game", 10, 10, "dat/tex/plains.png")
    game:addTextBox (-4.0, 0.0, "blop blop")
    switchWindow ( "menu1" )
    return 1
end
