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
function LuaWindow:addWorld (w,h,path)
    newWorld ((self.lwName),w,h,path)
end
LuaWindowElem = { lweName = "NULLname"
                , lweX = 0
                , lweY = 0 }
function LuaWindowElem:new (o)
    o = o or {}
    self.__index = self
    setmetatable (o, self)
    self.lweName = "NULL"
    return o
end
function LuaWindowElem:init (n,x,y)
    self.lweName = n
    self.lweX = x
    self.lweY = y
end
LuaWindowElemPiece = { lwep = "NULLname"
                     , elemType = "NULLtype" }
function LuaWindowElemPiece:new (o)
    o = o or {}
    self.__index = self
    setmetatable (o, self)
    self.lwep = "NULL"
    return o
end
function LuaWindowElemPiece:init (elemPieceType,args)
    self.elemType = elemPieceType
    self.lwep = args
end
function LuaWindow:addElement (lwe)
    newMenu ((self.lwName), (lwe.lweName), (lwe.lweX), (lwe.lweY))
end
function LuaWindowElem:addToElem (elemPiece)
    newMenuElement ((self.lweName), (elemPiece.elemType), (elemPiece.lwep))
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
    local paramsMenu = LuaWindowElem:new ()
    paramsMenu:init("paramsMenu1",-4.0,2.0)
    menu2:addElement (paramsMenu)
    local elemPiece = LuaWindowElemPiece:new ()
    elemPiece:init("text", "World Parameters")
    paramsMenu:addToElem(elemPiece)
    menu2:addButton (-4.0, -4.0, "back", menu1)
    game:addWorld ( 10, 10, "dat/tex/plains.png" )
    game:addTextBox (-4.0, 0.0, "blop blop")
    switchWindow ( "menu1" )
    return 1
end
