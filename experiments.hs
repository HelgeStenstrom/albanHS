
-- === experiment ============================
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


