-- ett försök att modellera Alban i Haskell.
-- Jag har ingen plan, utan jobbar på ett tag för att se hur långt jag kommer. 

-- Helge Stenström 2015-04-15
-- Helge Stenström 2015-05-21

-- Se även http://www.barrelfish.org/, eller kanske snarare
-- http://www.barrelfish.org/TN-002-Mackerel.pdf 

import Data.Bits


-- Registertyper
-- Alban SPI-register är 40 bitar.
-- PLL: 24 bitar
-- Crio: 16 bitar
-- Största enskilda fält är kanske 13 bitar.

-- Int är minst 30 bitar. Räcker därmed inte till Alban SPI-register.
-- Integer har godtyckligt antal bitar.



-- === Om fält och register =======
-- Fält har följande gemensamt:
--  . Kan representeras av ett heltal
--  . Har en LSB-position
--  . Har en ordlängd
--  . Ingår i ett register
--  Givet detta, bör man kunna skapa en funktion som gör ett 40- (16-
--  eller 24-) bitsord, med samma funktion oavsett vilket register det
--  handlar om.
--  För varje register kan man ha en lista med fält. Ordningen i
--  listan spelar egentligen ingen roll.

-- Register har följande:
--  . Fält (typer, fast)
--  . adress (fast)
--  . värden på fälten (ändras)



-- Hur man får fram värdet av VCO_F (a+b) beror på hur fälten definieras. 
-- valVcof (VcoF a b) = a + b
-- valVcoFa (VcoFa a) = 0

data Field = Field {value :: Int, lsbPos :: Int, width :: Int} deriving (Show)
-- Denna definition har nackdelen att värdet måste definieras. Jag kanske bara vill definiera lsbPos och width.

-- Exempelvis när man ska extrahera fält från ett registerord, givet
-- fältens vidd och position, har man inga värden i förväg. Man vill ge fälten värden från registerordet.

xfb = Field 8 5  5
xfa = Field 7 11 5

valField (Field v _ _ ) = v
lsbField (Field _ pos _) = pos
widthField (Field _ _ w) = w



-- Det känns problematiskt att man ändrar värden för registret
-- samtidigt som man kan påverka dess adress. Adressen är en del av
-- datatypen, eftersom vi inte får blanda ihop olika register.
-- Man kan inte kopiera från ett register till ett annat.
-- 
data Reg3 = Reg3 {
          fields :: [Field],
          address :: Address
          } deriving (Show)
-- xReg3 is VCO_F at address 1
vcofReg3 = Reg3 [xfa, xfb] 1

-- The value a field contributes with i a register word, based on its LSB position and its own value.
inPosition :: Field -> Int
inPosition f = shiftL ((valField f) .&. (shiftL 1 (widthField f) - 1))  (lsbField f)
-- Example: (inPosition xfa, inPosition xfb) = (14336,256)


valuesFieldList fs = map valField fs
xValuesFL = valuesFieldList[xfa, xfb]

wordFL :: [Field] -> Int
wordFL r = foldl (.|.) 0 (map inPosition r)

wordReg3 :: Reg3 -> Int
wordReg3 (Reg3 fs a) = (foldl (.|.) 0 (map inPosition fs)) .|. a

-- Alternativ definition
wordReg r =  (foldl (.|.) 0 (map inPosition (fields r))) .|. (address r)

-- Denna funktion är redundant. Funktionen fields definieras genom Reg3.
fieldsOf (Reg3 fs a) = fs

type Address = Int


-- Tilldela ett register sina värden från ett 16- eller 24-bitsord
-- . adressfältet avgör vilket register det handlar om
-- . Registertypen har sina unika fält
-- . Om fälten finns i en lista, kan man iterera över dem
-- . Kanske man inte behöver iterera. Kanske Haskells mönstermatchning
--   klarar biffen.
-- . Reg3 har en adressdel och en fältlista

addressCrioWord w = w .&. 15

exCWord w16 = 4

pllPart w40 = ((shiftL 1 24) -1) .&. (shiftR w40 16 )
crioPart w40 = 65535 .&. w40
