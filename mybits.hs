-- Testa diverse saker jag vill ha in i alban.hs så småningom

import Data.Bits

-- Typsignatur behövs av någon anledning.
z :: Int
x :: Integer
z = shift 1 16
x = shift 1 16
-- x = shift 1 16
-- x = shift (1+0) (16-2)

y = 2 + 3

data R1 = R1 Int String deriving (Show)
rs (R1 _ a) = a

type CustID = Int
type Rs2 = Int
data R2  = R2 {custID :: CustID,
              rs2 ::Rs2}
              deriving (Show)


data Field = Field { value :: Value, width :: Width, lsbPos :: LsbPos } deriving (Show)
type Value = Int
type Width = Int
type LsbPos = Int

data Register = Register [Field] deriving (Show)

fieldValue (Field value _ _) = value
fieldLsb (Field _ _ lsbPos) = lsbPos
fieldWidth (Field _ w _) = w
-- wordVal Regiser 
wordVal (Register r) = "ojsan"

regValues (Register [(Field val wid lsb)]) = 3
regValues _ = 17

fieldsOfReg (Register [f] ) = [f] 
fieldsOfReg _ = [f1] 

f1 = Field 1 1 8
f2 = Field 1 1 1
r1 = Register [f1, f2]

