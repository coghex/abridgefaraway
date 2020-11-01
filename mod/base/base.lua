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
-- this runs once at the beginning
function initLua ()
    newWindow ( "menu", "menu", "dat/tex/texture1.png" )
    newText ("menu", -4.0, 4.0, "A Bridge Far Away...")
    newButtonAction ("menu", -4.0, 2.0, "Create World", "link", "createParams")
    newButton ("menu", -4.0, 0.0, "Load World")
    newButtonAction ("menu", -4.0, -2.0, "Escape", "action", "quit")
    newWindow ( "createParams", "menu", "dat/tex/texture1.png" )
    newButtonAction ("createParams", -4.0, -2.0, "Create World", "link", "game")
    newMenu ("createParams", "createParamsMenu", -4.0, 2.0)
    newMenuElement ("createParamsMenu", "text", "World Parameters")
    newButtonAction ("createParams", -4.0, -4.0, "back", "link", "menu")
    newWindow ( "game", "game", "dat/tex/black.png" )
    newWorld ( "game", 10, 10, "dat/tex/plains.png")
    switchWindow ( "menu" )
    return 1
end
