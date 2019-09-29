import Data.List
------- Codificacao

let2int :: Char -> Int
let2int x = ( fromEnum x ) - 97

int2let :: Int -> Char
int2let x = ( toEnum ( x + 97 ) )

-- If the shift is greater than 25 there is more than 1 turn => Subtract 26 from the shift number
-- If the shift is greater thant the distance to the alphaet's end => Use the remaining shift - 1 where the remaining shift is shift - distance to the end 
shift :: Int -> Char -> Char
shift sh ' ' = ' '
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

chisqr :: [Float] -> [Float] -> Float
chisqr [] _ = 0.0
chisqr _ [] = 0.0
chisqr ( x:xs ) ( y:ys ) = ( ( ( x - y ) ^ 2 ) / y ) + chisqr xs ys 

ex = map (\x -> 1) [1.0 .. 26.0]
-- λ> chisqr (freqs "abbcccddddeeeee") (map fromInteger ex)

rotate :: Int -> [ a ] -> [ a ]
rotate 0 as = as
rotate n as = right ++ left
    where left = take n as
          right = drop n as 

crack :: String -> String
crack a = encode ( 26 - rotationNumber ) a 
    where table' = freqs a
          tableChisqr = rotateChisqr 0 tableEnglish table'
          rotationNumber = (\(Just x) -> x) (elemIndex ( minimum tableChisqr ) tableChisqr)    

tableEnglish = [ 8.167, 1.492, 2.782, 4.253, 12.702, 2.228, 2.015, 6.094, 6.966, 0.153
               , 0.772, 4.025, 2.406, 6.749, 7.507, 1.929, 0.095, 5.987, 6.327, 9.056
               , 2.758, 0.978, 2.360, 0.150, 1.974, 0.074 ] :: [Float]

rotateChisqr :: Int -> [Float] -> [Float] -> [Float]
rotateChisqr 26 _ _ = []
rotateChisqr n table table'= ( chisqr ( rotate n table' ) table ) : rotateChisqr ( n + 1 ) table table'


