import Data.List (elemIndex)
import Data.Maybe (fromJust)

type Message = String
type SquareCipher = [[Char]]

-- Function to find the index of a character in the square cipher
findIndex :: Char -> SquareCipher -> (Int, Int)
findIndex ch cipher =
  let Just rowIndex = elemIndex ch (concat cipher)
      colIndex = rowIndex `mod` 6
      rowIndex' = rowIndex `div` 6
  in (rowIndex' + 1, colIndex + 1)

-- Function to encrypt a character using the square cipher
encryptChar :: Char -> SquareCipher -> Char
encryptChar ch cipher =
  let (row, col) = findIndex ch cipher
  in cipher !! (col - 1) !! (row - 1)

-- Function to encrypt a message using the square cipher
cypher :: Message -> Message
cypher msg = map (\ch -> if ch `elem` (concat squareCipher) then encryptChar ch squareCipher else ch) msg
  where
    squareCipher = ["abcdef", "ghijkl", "mnopqr", "stuvwx", "yz,;.:","-!?+/∗"]