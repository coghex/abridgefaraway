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
    newText ("menu", -4.0, 4.0, "A Bridge Far Away...")
    newButton ("menu", -4.0, 2.0, "Create World")
    newButton ("menu", -4.0, 0.0, "Load World")
    newButton ("menu", -4.0, -2.0, "Escape")
    newTile ( "createParams", 0.0, 0.0, "dat/tex/texture2.png" )
    switchWindow ( "createParams" )
    return 1
end
