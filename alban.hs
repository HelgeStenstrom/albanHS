-- ett försök att modellera Alban i Haskell.
-- Jag har ingen plan, utan jobbar på ett tag för att se hur långt jag kommer. 

-- Helge Stenström 2015-04-15

-- Se även http://www.barrelfish.org/, eller kanske snarare
-- http://www.barrelfish.org/TN-002-Mackerel.pdf 

import Data.Bits


-- Registertyper
-- Alban SPI-register är 40 bitar.
-- PLL: 24 bitar
-- Crio: 16 bitar
-- Största enskilda fält är kanske 13 bitar.

-- Tills vidare antar jag att Int är 64 bitar, och därmed räcker.




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


-- ====== Crio fields of registers =====================
-- Misc fields
type AdcEn = Field
type Vswr = Field
type Vswpc = Field
type Spi = Field
type Divider = Field
type PllBufI = Field
type Pon = Field

-- Vcof fields
type VcoFa = Field
type VcoFb = Field

-- Vcoi fields
type Core_select = Field
type Vco_en = Field
type I_max = Field

-- Agc fields
type Agc_speed = Field
type Agc_gain = Field
type Agc_target = Field

-- Buf fields
type Mute_div = Field
type Mute_buf = Field
type Mute = Field
type Buf_i = Field

-- Mux fields
type Pll_x2 = Field
type Alarm_oc = Field
type Swing_force = Field
type Temp_force = Field
type Ld_pol = Field
type Alarm_pol = Field
type Mux_val = Field  -- Name clash with register name Mux.

-- Vtok fields
type Temp_r = Field
type Vtok_hi = Field
type Vtok_low = Field

-- Amplok fields
type Amplok_hi = Field
type Amplok_low = Field


-- ========= PLL fields and registers ==================================
-- fi: Function latch and initialization latch
type P   = Field
type Pd2 = Field
type Cp2 = Field
type Cp1 = Field
type Tc  = Field
type F   = Field 
type M   = Field
type PD1 = Field
type F1  = Field

-- R: Reference counter latch
type R   = Field
type Abp = Field
type Ldp = Field
type T   = Field

-- N: N counter latch
type G   = Field
type B   = Field
type A   = Field



data Misc = Misc {
                  adcEn   :: AdcEn,
                  vsvr    :: Vswr,
                  vswpc   :: Vswpc,
                  spi     :: Spi,
                  divider :: Divider,
                  pllBufI :: PllBufI,
                  pon     :: Pon }
            deriving (Show)

data VcoF = VcoF {
                  vcoFa :: VcoFa,
                  vcoFb :: VcoFb } 
            deriving (Show)

data VcoI = VcoI {
                  core_select :: Core_select,
                  vco_en      :: Vco_en,
                  i_max       :: I_max}
            deriving (Show)

data Agc = Agc {
                agc_speed :: Agc_speed,
                agc_gain  :: Agc_gain,
                agc_target :: Agc_target }
           deriving (Show)

data Buf = Buf {
                mute_div :: Mute_div,
                mute_buf :: Mute_buf,
                mute     :: Mute,
                buf_i    :: Buf_i }
           deriving (Show)

data Mux = Mux {
                pll_x2      :: Pll_x2,
                alarm_oc    :: Alarm_oc,
                swing_force :: Swing_force,
                temp_force  :: Temp_force,
                ld_pol      :: Ld_pol,
                alarm_pol   :: Alarm_pol,
                mux_val     :: Mux_val -- Name clash with register name Mux
               }

           deriving (Show)

data Vtok = Vtok {
                  temp_r     :: Temp_r,
                  vtok_hi  :: Vtok_hi,
                  vtok_low :: Vtok_low
                  } deriving (Show)

data Amplok = Amplok {
                      amplok_hi  :: Amplok_hi,
                      amplok_low :: Amplok_low
                     } deriving (Show)

-- ===================================================




data CrioRegs = CrioRegs {
              misc :: Misc,
              vcof :: VcoF,
              vcoi :: VcoI,
              agc  :: Agc,
              buf  :: Buf,
              mux  :: Mux,
              vtok :: Vtok,
              amplok :: Amplok
              } deriving (Show)

-- Hur man får fram värdet av VCO_F (a+b) beror på hur fälten definieras. 
-- valVcof (VcoF a b) = a + b
-- valVcoFa (VcoFa a) = 0

data Field = Field {value :: Int, lsbPos :: Int, width :: Int} deriving (Show)
-- Denna definition har nackdelen att värdet måste definieras. Jag kanske bara vill definiera lsbPos och width.

-- Exempelvis när man ska extrahera fält från ett registerord, givet
-- fältens vidd och position, har man inga värden i förväg. Man vill ge fälten värden från registerordet.

xfb = Field 1 5  5
xfa = Field 1 11 5
xReg = VcoF xfa xfb

valField (Field v _ _ ) = v
lsbField (Field _ pos _) = pos
widthField (Field _ _ w) = w

valVcof (VcoF a b ) = valField a + valField b
type Reg = [Field]


type Vcof2 = Reg

xReg2 = [xfa, xfb]

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
xReg3 = Reg3 [xfa, xfb] 1

-- The value a field contributes with i a register word, based on its LSB position and its own value.
inPosition :: Field -> Int
inPosition f = shiftL ((valField f) .&. (shiftL 1 (widthField f) - 1))  (lsbField f)

valuesReg r = map valField r
xValuesReg = valuesReg[xfa, xfb]

wordReg :: [Field] -> Int
wordReg r = foldl (.|.) 0 (map inPosition r)

wordReg3 :: Reg3 -> Int
wordReg3 (Reg3 fs a) = (foldl (.|.) 0 (map inPosition fs)) .|. a

type Address = Int
type W16 = Int


-- data VcoF4 = VcoF4 {vcof4 :: Reg3} deriving (Show)

-- data CrioReg fs address = VcoF4 fs 1

-- Tilldela ett register sina värden från ett 16- eller 24-bitsord
-- . adressfältet avgör vilket register det handlar om
-- . Registertypen har sina unika fält
-- . Om fälten finns i en lista, kan man iterera över dem
-- . Kanske man inte behöver iterera. Kanske Haskells mönstermatchning
--   klarar biffen.
-- . Reg3 har en adressdel och en fältlista

addressCrioWord w = w .&. 15

exCWord w16 = 4

data JValue = JString String
            | JNumber Double
            | JNull
            deriving (Eq, Ord, Show)
getString (JString s) = Just s
getString _           = Nothing
getInt    (JNumber n) = Just (truncate n)
getInt    _           = Nothing
isNull    v           = v == JNull

-- ==========================================================================

pllPart w40 = ((shiftL 1 24) -1) .&. (shiftR w40 16 )
crioPart w40 = 65535 .&. w40



-- === experiment ============================


oddOrEven n = if odd n
          then "odd"
          else "even"

-- Jag försöker komma på hur man går från ett allmänt 16-bit värde, till ett specifikt register, med sin egen typ.
regType w16 = if w16 .&. 15 == 3
        then "tre"
        else "inte tre"

-- Exempel: Färgkod på motstånd
data Färg = Svart
          | Brun
          | Röd
          | Orange
          | Gul
          | Grön
          | Blå
          | Lila
          | Grå
          | Vit
          deriving (Show)

färgNummer 2 = Röd
färgNummer 1 = Brun
färgNummer 0 = Svart
färgNummer 3 = Orange
färgNummer 4 = Gul
färgNummer 5 = Grön
färgNummer 6 = Blå
färgNummer 7 = Lila
färgNummer 8 = Grå
färgNummer 9 = Vit


