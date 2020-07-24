sayHello :: String -> IO ()
sayHello x = putStrLn $ "Hello, " ++ x ++ "!"

triple :: (Num a) => a -> a
triple x = x * 3

sumList :: (Num a) => [a] -> a
sumList = sum

x = 10 * 5 + y

myResult = x * 5

y = 10

λ = 12

safeDiv :: Double -> Double -> Maybe Double
safeDiv x y | y == 0 = Nothing
            | otherwise = Just $ x / y

computeDiv x y z a b c = do let z' = x + y
                                c' = a + b
                            z'' <- safeDiv z' z
                            c'' <- safeDiv c' c
                            return $ z'' + c''

testExp = let x = 5; y=6 in x+3
