-- ett försök att modellera Alban i Haskell.
-- Jag har ingen plan, utan jobbar på ett tag för att se hur långt jag kommer. 

-- Helge Stenström 2015-04-15
-- Helge Stenström 2015-05-21

-- Se även http://www.barrelfish.org/, eller kanske snarare
-- http://www.barrelfish.org/TN-002-Mackerel.pdf 

import Data.Bits

-- Imports of QuickCheck are in conflict with imports of Data.Bits.
-- Vad kan jag göra åt det?
-- Specifikt: .&. finns i båda.

-- import Test.QuickCheck
-- import Test.QuickCheck.All

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

data Field = Field {value :: Int,
                   lsbPos :: Int,
                   width :: Int}
                   deriving (Show, Eq)
-- Denna definition har nackdelen att värdet måste definieras. Jag kanske bara vill definiera lsbPos och width.

-- Exempelvis när man ska extrahera fält från ett registerord, givet
-- fältens vidd och position, har man inga värden i förväg. Man vill ge fälten värden från registerordet.

xfb = Field 8 5  5
xfa = Field 7 11 5

valField (Field v _ _ ) = v
lsbField (Field _ pos _) = pos
widthField (Field _ _ w) = w

type RegContents = [Field]

-- Det känns problematiskt att man ändrar värden för registret
-- samtidigt som man kan påverka dess adress. Adressen är en del av
-- datatypen, eftersom vi inte får blanda ihop olika register.
-- Man kan inte kopiera från ett register till ett annat.
-- 
data Reg = Reg {
          fields :: RegContents,
          address :: Address
          } deriving (Show, Eq)

vcofRegExample = Reg [xfa, xfb] 1
vcofReg fs = Reg fs 1
-- type VcofReg fs = Reg fs 1

-- Ordningen i första argumentet spelar roll:
-- vcofRegExample == vcofReg [xfa, xfb]
-- vcofRegExample /= vcofReg [xfb, xfa]


data CrioReg a = VcoF Reg a
               | VcoI Reg a
                 deriving (Show)

data CrioReg2 a  = MiscReg a
              | VcoFReg a
              | VcoIReg a
                deriving (Show)

crioReg w16
        | addressCrioWord w16 == 1 = VcoFReg 1


-- The value a field contributes with i a register word, based on its LSB position and its own value.
inPosition :: Field -> Int
inPosition f = shiftL ((valField f) .&. (shiftL 1 (widthField f) - 1))  (lsbField f)
-- Example: (inPosition xfa, inPosition xfb) = (14336,256)


valuesFieldList fs = map valField fs
xValuesFL = valuesFieldList[xfa, xfb]

wordFL :: RegContents -> Int
wordFL r = foldl (.|.) 0 (map inPosition r)

wordReg :: Reg -> Int
wordReg (Reg fs a) = (foldl (.|.) 0 (map inPosition fs)) .|. a

-- Alternativ definition
wordReg2 :: Reg -> Int
wordReg2 r =  (foldl (.|.) 0 (map inPosition (fields r))) .|. (address r)

-- Denna funktion är redundant. Funktionen fields definieras genom Reg.
fieldsOf (Reg fs a) = fs

type Address = Int


-- Tilldela ett register sina värden från ett 16- eller 24-bitsord
-- . adressfältet avgör vilket register det handlar om
-- . Registertypen har sina unika fält
-- . Om fälten finns i en lista, kan man iterera över dem
-- . Kanske man inte behöver iterera. Kanske Haskells mönstermatchning
--   klarar biffen.
-- . Reg har en adressdel och en fältlista

addressCrioWord w = w .&. 15

exampleCrioWord w16 = 4

pllPart w40 = ((shiftL 1 24) -1) .&. (shiftR w40 16 )
crioPart w40 = 65535 .&. w40

nameOfCrioWord w16 
    | addressCrioWord w16 == 1 = "VCO_F"
    | addressCrioWord w16 == 2 = "VCO_I"
    | otherwise = "not yet defined"
                                 

f1ofCrioWord w16 
    | addressCrioWord w16 == 1 = Just "VCO_F"
    | otherwise = Nothing

f2ofCrioWord w16 
    | addressCrioWord w16 == 1 = Just (Reg [xfa, xfb] 1)
    | otherwise = Nothing

f3ofCrioWord w16 
    | addressCrioWord w16 == 1 = Reg [xfa, xfb] 1
    | otherwise = Reg [xfb, xfa] 17


data SomeField  = FaField Field  
                | FbField Field
                  deriving (Show)

f4ofCrioWord w16 
    | addressCrioWord w16 == 1 = Reg [Field 8 5 5, Field 7 11 5] 1

f5ofCrioWord w16 
    | addressCrioWord w16 == 1 = Reg [Field valFb 5 5, Field valFa 11 5] 1
    where 
      valFa = (shiftR w16 11) .&. 31
      valFb = (shiftR w16 5) .&. 31

-- Denna funktion lämnar ifrån sig register Reg av en dålig typ, för
-- den är ganska anonym. De olika registren skiljer sig åt enbart
-- genom sitt datainnehåll. Inte genom sin typ.
-- Dessutom blir det onödigt mycket att skriva. 
f6ofCrioWord w16 
    | addressCrioWord w16 == 1 = Reg [Field valFb posB 5, Field valFa posA 5] 1
    | addressCrioWord w16 == 2 = Reg [Field valCoreSel posCoreSel 2] 2
    where 
      valFa = (shiftR w16 posA) .&. 31
      valFb = (shiftR w16 posB) .&. 31
      posA = 11
      posB = 5
      valCoreSel = (shiftR w16 posCoreSel) .&. 3
      posCoreSel = 10


-- Jag vill ha olika register som är olika typer, men ändå inte. De
-- har genemsamma nämnare.
-- Varför vill jag ha olika typer? För att jag tror att det ökar säkerheten. 

type Blå = Int
type Röd = Int

f7 1 = 1::Blå
f7 2 = 2::Röd

main = putStrLn "Hej, världen!"
huvud = putStrLn "Detta är funktionen 'huvud'."





-- Idéer från #haskell, användare Hijiri:
-- Don't store the length and position with the data.
-- Have a type for each register type.
-- and then for your interpreting function output a sum type that includes all of them as alternatives
-- like data PossibleRegisters = R1 RegisterType1 | R2 RegisterType2 | ...
-- and interpret :: Word16 -> PossibleRegisters
-- or something like that
-- and then with each constructor you know what register type it has, and you can choose to do something different based on that
-- Hijiri: So my interpret function would take the 4 LSB, and then
-- return different RegisterType1, RegisterType2 etc, depending on
-- them. And these RegisterTypeN values would take their data
-- depending on the 12 MSB.
-- 22:44:16 Hijiri: HelgeS: yep
-- Hijiri: though (pedantically) the interpret function will be returning the big sum type that has those RegisterN types as alternatives
-- Hijiri: it won't be returning those types directly
-- HelgeS: Hijiri: Now, my registers consist of fields of various lengths and LSB positions, and the same names, lengths and LSB positions could be used for the functions that construct a 16-bit register value from a register. And I don't to define these lengths and LSB positions in more than one place.
-- do you mean you don't want to define those numbers in more than one place?
--  for that you could just define them at the top level
--  HelgeS: My idea is to have "data Reg a = FieldA Reg a | FieldB a deriving (Show)"
--  HelgeS: Hijiri: My idea is that for example one register, called
--  RegA has two fields, called FieldA and FieldB, that both share the
--  properties that fields have, such as a length in bits, an LSB
--  position within the register word, and (possibly) a value.
--  Hijiri: HelgeS: Is Reg a supposed to be a register?
--  HelgeS: Yes
--  Hijiri: it wouldn't make sense to define it as FieldA (Reg a) | FieldB then, since that would mean it either has FieldA or FieldB, not both
--  Hijiri: what I would do is first define a Field type
--  Hijiri: actually wait
--  HelgeS: I'm sorry, I think I have to take a pause here and read my code carefully again.
--  HelgeS: I don't remember all my intentions with the code.
--  Hijiri: why do you need to store the length and LSB position in the data?
--   Hijiri: it seems like that should be handled by the interpret and encoding functions
--   HelgeS: Hijiri: Perhaps I don't have to do that. But I want to do
--   conversions in both directions, between arbitarary 16-bit words
--   and well-typed registers with sub-fields. All 16 registers
--   together comprise most of the state of the modelled circuit.
--  Hijiri: HelgeS: the functions in both directions can each use the length and position data
--  Hijiri: you don't need to store the length and position data because you already know them from what kind of register it is
--  HelgeS: Good points!
