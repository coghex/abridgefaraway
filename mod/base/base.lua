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
    newWindow ( "menu", "dat/tex/texture1.png" )
    newWindow ( "createParams", "dat/tex/black.png" )
    newWindow ( "game", "dat/tex/black.png" )
    newText ("menu", -4.0, 4.0, "A Bridge Far Away...")
    newButtonAction ("menu", -4.0, 2.0, "Create World", "link", "createParams")
    newButtonAction ("createParams", -4.0, -2.0, "Create World", "link", "game")
    newButtonAction ("createParams", -4.0, -4.0, "back", "link", "menu")
    newButton ("menu", -4.0, 0.0, "Load World")
    newButtonAction ("menu", -4.0, -2.0, "Escape", "action", "quit")
    newTile ( "game", 0.0, 0.0, "dat/tex/texture2.png" )
    newTile ( "game", 1.0, 1.0, "dat/tex/texture1.png" )
    switchWindow ( "menu" )
    return 1
end
