main :: IO ()
main = putStrLn "Test suite not yet implemented"

-- Example non-minimal DFA:
-- ['a', 'b', 'c', 'd', 'e', 'f']
-- [0, 1]
-- [('a', Just 0, 'b'),('a', Just 1, 'c'),('b', Just 0, 'a'),('b', Just 1, 'd'),('c', Just 0, 'e'),('c', Just 1, 'f'),('d', Just 0, 'e'),('d', Just 1, 'f'),('e', Just 0, 'e'),('e', Just 1, 'f'),('f', Just 0, 'f'),('f', Just 1, 'f')]
-- ['a']
-- ['c', 'd', 'e']

-- Should be:
-- [[],[["a","b"]],[["c","d","e"]]]
-- [0,1]
-- [([],0,[]),([],1,[]),([["a","b"]],0,[["a","b"]]),([["a","b"]],1,[["c","d","e"]]),([["c","d","e"]],0,[["c","d","e"]]),([["c","d","e"]],1,[])]
-- [["a","b"]]
-- [[["c","d","e"]]]
