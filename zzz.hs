import Text.Printf(printf)

{-|
  The 'zzz' function calulates the final capital from
  a start capital 'v', an anual saving rate 'a', an average return rate 'z' and a duration of 'j' years.   
-} 
zzz :: Double -> Double -> Double -> Int -> Double
zzz v a z j | j == 0 = v
            | otherwise = zzz (v * z + a) a z (j - 1)

{-|
  The 'zz' function shows the compound interest effect on an anual base. It uses the 'zzz' function.
-}
zz :: Double -> Double -> Double -> Int -> [(Int,Double)]
zz v a z j = [(i, zzz v a z i) | i <- [0..j]]

{-|
  The 'z' function prints the results of the 'zzz' and 'zz' functions and their arguments in a pretty way. 
-}
z :: Double -> Double -> Double -> Int -> IO()
z v a z j = putStrLn "COMPOUND INTEREST CALCULATOR\n"
            >> putStrLn ( printf "starting capital:%.0f\n\
                              \anual saving rate:%0.f\n\
                              \average return:%1.3f\n\
                              \duration:%d\n\
                              \final capital:%.0f\n" v a z j final)
            >> putStrLn "details:"
            >> putStrLn (s details)
  where s []         = ""
        s ((j,v):xs) = printf format j v ++ "\n" ++ s xs
        final        = zzz v a z j
        details      =  zz v a z j
        maxResult    = maximum [r | (_,r) <- details]
        format       = "%" ++ (show $ digCnt 1 j) ++ "d | "
                       ++ "%" ++ (show $ digCnt 1 $ round maxResult) ++ ".0f"
        digCnt ds n  | n >= 10   = digCnt (ds + 1) (n `div` 10)
                     | otherwise = ds


sample = z 8000 700 1.04 5
