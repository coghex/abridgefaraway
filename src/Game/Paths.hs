module Game.Paths where

getDataFileName :: FilePath -> IO FilePath
getDataFileName fileName = return $ "data/" ++ fileName
