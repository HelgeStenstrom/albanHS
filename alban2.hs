-- Nystart med Alban, utan att snegla på vad jag gjort förut.

import Data.Bits ((.&.), shiftL, shiftR)

-- === Generic types ===
-- A Field holds a single value, 1 to 14 bits.
-- It has other properties too, such as width, position and which
-- register it belongs to, but we don't handle that yet.
type Field = Int

-- A Register consists of several fields. It's not just a list of
-- fields, but very specific fields.
type Register = [Field]

-- The Crio part of Alban has several registers.
type CrioRegs = [Register]

-- The Crio part of Alban has several unique registers.
-- these should perhaps be parametrized.
data CrioRegs2 = CrioRegs Misc VcoF VcoI Agc deriving Show

-- These named registers have different addresses, width and positions.
type Misc = Register
type VcoF = Register
type VcoI = Register
type Agc  = Register

data RegEx = RegEx {field1 :: Field }

-- The PLL part of Alban has several registers.
type PllRegs = [Register]

-- The Crio part of Alban has several unique registers.
-- these should perhaps be parametrized.
data CrioReg = CRegZero Int 
             | CRegOne Int 
             | CRegTwo Int 
             | CRegThree Int deriving Show
-- example usage:
exCregThree :: CrioReg
exCregThree = CRegThree 42


-- === 
-- an Int is not (guaranteed to be) large enough to hold a 40-bit word. 
type W40 = Integer

-- === Parse data words ====
-- Parsing the 40-bit Alban data word is easy, because everything has
-- fixed positions.

-- The PLL address of an Alban word is bits # to #
pllAddress :: Int -> Int
pllAddress  n = n .&. 3

-- The PLL part of a w40 word is bits 39 to 16 (24 bits)
pllPart :: W40 -> Int
pllPart n = fromInteger $ (2^wordlength-1) .&. (shiftR n pos)
  where wordlength = 24
        pos = 16

-- The Crio address of an Alban word is bits 3 to 0
crioAddress :: Int -> Int
crioAddress  n = n .&. 15

-- The Crio of a w40 word is bits 15 to 0
crioPart :: W40 -> Int
crioPart x = fromInteger $ x .&. (2^16-1) 

-- testing that the w40 can be split and put together again.
propCrioPll1 n = toInteger (shiftL (pllPart n) 16 + crioPart n) == n
propCrioPll2 n = shiftL (pllPart n) 16 + crioPart n == fromInteger n

parseCrio :: Int -> CrioReg
parseCrio n | c n == 0 = CRegZero n -- MISC
            | c n == 1 = CRegOne n  -- VCO_F
            | c n == 2 = CRegTwo n  -- VCO_I
            | c n == 3 = CRegThree n -- AGC
  where c = crioAddress
