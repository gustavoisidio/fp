percentOf1Letter
------- Codificacao

let2int :: Char -> Int
let2int x = ( fromEnum x ) - 97

int2let :: Int -> Char
int2let x = ( toEnum ( x + 97 ) )

-- If the shift is greater than 25 there is more than 1 turn => Subtract 26 from the shift number
-- If the shift is greater thant the distance to the alphaet's end => Use the remaining shift - 1 where the remaining shift is shift - distance to the end 
shift :: Int -> Char -> Char
shift sh x
    | sh > 25 = shift ( sh - 26 ) x
    | sh > disToTheEnd = int2let $ ( sh - disToTheEnd - 1 ) 
    | otherwise = int2let $ ( let2int x ) + sh
    where disToTheEnd = abs ( ( let2int x ) - 25 )  

encode :: Int -> String -> String
encode _ [] = []
encode sh ( a:as ) = ( shift sh a ) : encode sh as

------- Decodificacao

percent :: Int -> Int -> Float
percent x y = 100.0 * ( (fromIntegral x) / ( fromIntegral y ) )

freqs :: String -> [ Float ]
freqs a = returnFreq ['a'..'z'] a

returnFreq :: String -> String -> [Float]
returnFreq [  ] _ = []
returnFreq ( a:as ) bs = percentOf1Letter : recursiveCall  
    where repeatCount = length [x | x  <- bs, x == a]
          percentOf1Letter = ( percent repeatCount ( length bs ) )
          recursiveCall = returnFreq as bs


rotate :: Int -> [ a ] -> [ a ]
rotate n as = right ++ left
    where left = take n as
          right = drop n as 







