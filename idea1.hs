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

xReg = VcoF xfa xfb
valVcof (VcoF a b ) = valField a + valField b
