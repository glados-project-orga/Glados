{-
-- EPITECH PROJECT, 2025
-- glados-repo
-- File description:
-- help
-}

module Help (
    helpMessage
    ) where

helpMessage :: String
helpMessage = unlines
    [
        "Usage: inklusif [[File], [-help]]",
        "",
        "Options:",
        " -help      Show this help message",
        " File       Your file muse be an Inklusif type file (helloworld.ink)",
        "",
        "For more information for inklusif language, visit the documentation."
    ]

