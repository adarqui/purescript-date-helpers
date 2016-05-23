module Data.Date.Helpers (
    Date
  , underlyingDate
  , now
  , dateFromString
  , defaultDate
  , year
  , month
  , dayOfMonth
  , unYear
  , unDayOfMonth
  , readDate
  , toISOString
  , yyyy_mm_dd
  , failDate
) where



import Control.Monad.Eff    (Eff)
import Data.Argonaut.Core   (toString)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Enum            (fromEnum)
import Data.Either          (Either(Left,Right))
import Data.Foreign         (F, Foreign, ForeignError(TypeMismatch), tagOf, unsafeReadTagged)
import Data.Foreign.Class   (class IsForeign)
import Data.Function        (Fn2, runFn2, on)
import Data.JSON            (class ToJSON, class FromJSON, JValue(JString), fail)
import Data.String          as Str
import Data.Date            as D
import Data.Date.UTC        as U
import Data.Time            as T
import Data.Maybe           (Maybe (..))
import Data.Maybe.Unsafe    (fromJust)
import Prelude              ( class Show, class Ord, class Eq
                            , show, compare, eq, pure, bind
                            , ($), (+), (<<<), (<$>), (++), (/=))



-- Thanks to Matt Parsons.
--
-- Most of the original code in this module can be found here:
-- https://github.com/parsonsmatt/ql-purs/tree/master/src/Types



newtype Date = Date D.Date



instance dateEq :: Eq Date where
  eq = eq `on` underlyingDate



instance dateOrd :: Ord Date where
  compare = compare `on` underlyingDate



instance dateShow :: Show Date where
  show = toISOString



instance isForeignDate :: IsForeign Date where
  read = readDate



instance dateFromJSON :: FromJSON Date where
  parseJSON (JString s) =
    case dateFromString s of
         Nothing -> failDate
         Just d  -> pure d
  parseJSON _           = failDate



instance dateToJSON :: ToJSON Date where
  toJSON d = JString (toISOString d)



instance dateDecodeJson :: DecodeJson Date where
  decodeJson json =
    case toString json of
         Nothing -> failDate
         Just d  -> case dateFromString d of
                         Nothing -> failDate
                         Just d'  -> pure d'



instance dateEncodeJson :: EncodeJson Date where
  encodeJson d = encodeJson (toISOString d)



foreign import jsDateMethod :: forall a. Fn2 String D.JSDate a



underlyingDate :: Date -> D.Date
underlyingDate (Date d) = d



now :: forall e. Eff (now :: D.Now | e) Date
now = Date <$> D.now



dateFromString :: String -> Maybe Date
dateFromString str = Date <$> D.fromString str



defaultDate :: Date
defaultDate = Date (fromJust $ D.fromString "1982-01-01T05:00:00.000Z")



year :: Date -> D.Year
year = U.year <<< underlyingDate



month :: Date -> D.Month
month = U.month <<< underlyingDate



dayOfMonth :: Date -> D.DayOfMonth
dayOfMonth = U.dayOfMonth <<< underlyingDate



unYear :: D.Year -> Int
unYear (D.Year n) = n



unDayOfMonth :: D.DayOfMonth -> Int
unDayOfMonth (D.DayOfMonth n) = n



threeDecimalCandidate :: String -> Maybe D.Date
threeDecimalCandidate s =
  D.fromString $ Str.drop 4 $ Str.takeWhile (/= '.') s



readDate :: Foreign -> F Date
readDate f =
  case tagOf f of
       "Date" ->
         case D.fromJSDate <$> unsafeReadTagged "Date" f of
              Right (Just d) -> Right (Date d)
              Right Nothing -> Left (TypeMismatch "invalid date" "asdf")
              Left a -> Left a
       "String" -> do
         e_s <- unsafeReadTagged "String" f
         case e_s of
           Left a  -> Left a
           Right s ->
             case D.fromString s of
               Just d  -> Right (Date d)
               Nothing ->
                 -- weird scenario: Perhaps s is of the form: 2015-09-15T05:19:18.556641000000Z
                 -- everything (your phone, browser, etc) works fine.. except your kindle paperwhite.
                 -- for some reason this device wants: 2015-09-15T05:19:18.556 (3 decimal places)
                 -- that's what this weird piece of extra logic/parsing addresses:
                 -- https://en.wikipedia.org/wiki/ISO_8601#Times
                 -- hdgarrood is always a big help with date issues
                 case threeDecimalCandidate s of
                   Nothing -> Left (TypeMismatch "invalid date" "invalid date")
                   Just d  -> Right (Date d)

       "Number" ->
         case D.fromEpochMilliseconds <<< T.Milliseconds <$> unsafeReadTagged "Number" f of
              Right (Just d) -> (Right (Date d))
              Right Nothing -> Left (TypeMismatch "invalid read" "expecting epoch milliseconds")
              Left a -> Left a
       _ ->
         Left (TypeMismatch "Expecting date" (tagOf f))



toISOString :: Date -> String
toISOString (Date d) = runFn2 jsDateMethod "toISOString" (D.toJSDate d)



yyyy_mm_dd :: Date -> String
yyyy_mm_dd (Date date) = y ++ "-" ++ m ++ "-" ++ d
  where
    y = (ypad <<< show) case U.year date of D.Year n -> n
    m = pad (1 + (fromEnum $ U.month date))
    d = pad case U.dayOfMonth date of D.DayOfMonth day -> day
    pad n = let str = show n
             in case Str.length str of
                   1 ->  "0" ++ str
                   _ -> str
    ypad str =
      case Str.length str of
           0 -> "0000"
           1 -> "000" ++ str
           2 -> "00" ++ str
           3 -> "0" ++ str
           _ -> str



failDate :: forall a. Either String a
failDate = fail "Could not parse Date."
