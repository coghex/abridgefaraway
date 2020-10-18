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
    --newText (1, 1, 1, "A Bridge Far Away...")
    --newButton (1, 2, 2, "Create World")
    --newButton (1, 2, 3, "Load World")
    --newButton (1, 2, 4, "Escape")
    return 1
end
