module Test where

main :: IO ()
main = getLine >>= \line1 ->
       getLine >>= \line2 ->
       putStrLn line2 >>= \_ ->
       putStrLn line1

