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
    return 1
end