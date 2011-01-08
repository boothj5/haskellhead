printPlayers :: String -> String
printPlayers p = "ok, " ++ p ++ " players."

main = do
    putStrLn "Shit head"
    putStrLn "How may players?"
    players <- getLine
    putStrLn $ printPlayers players
