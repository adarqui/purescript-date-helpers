module Data.Date.Helpers (
    Date
  , underlyingDate
  , dateFromString
  , defaultDate
  , year
  , month
  , DayOfMonth (..)
  , dayOfMonth
  , threeDecimalFix
  , threeDecimalCandidate
  , readDate
  , yyyy_mm_dd
  , failDate
  , toISOStringNoEff
) where



import Control.Monad.Eff        (Eff, kind Effect)
import Control.Monad.Except     (runExcept, throwError)
import Data.Argonaut.Core       (toString)
import Data.Argonaut.Decode     (class DecodeJson)
import Data.Argonaut.Encode     (class EncodeJson, encodeJson)
import Data.Enum                (fromEnum)
import Data.Either              (Either(Left,Right))
import Data.Foreign             (F, Foreign, ForeignError(TypeMismatch), tagOf, unsafeReadTagged, fail)
import Data.Foreign.Class       (class Decode)
import Data.Function.Uncurried  (Fn2(), runFn2)
import Data.Function            (on)
import Data.JSDate              (JSDate, toDateTime, fromDateTime)
import Data.String              as Str
import Data.Date                as D
import Data.DateTime            (DateTime, date)
import Data.Time.Duration       (Milliseconds(Milliseconds)) as T
import Data.Maybe               (Maybe (..), fromJust)
import Prelude                  (class Show, class Ord, class Eq
                                ,show, compare, eq, pure
                                ,($), (+), (<<<), (<$>), (==), (<>))



-- Thanks to Matt Parsons.
--
-- Most of the original code in this module can be found here:
-- https://github.com/parsonsmatt/ql-purs/tree/master/src/Types



newtype Date = Date DateTime



instance dateEq :: Eq Date where
  eq = eq `on` underlyingDate



instance dateOrd :: Ord Date where
  compare = compare `on` underlyingDate



instance dateShow :: Show Date where
  show (Date dt) = toISOStringNoEff (fromDateTime dt)



instance decodeData :: Decode Date where
  decode = readDate



instance dateDecodeJson :: DecodeJson Date where
  decodeJson json =
    case toString json of
         Nothing -> failDate
         Just d  -> case dateFromString d of
                         Nothing -> failDate
                         Just d'  -> pure d'



instance dateEncodeJson :: EncodeJson Date where
  encodeJson (Date dt) = encodeJson (toISOStringNoEff $ fromDateTime dt)



underlyingDate :: Date -> DateTime
underlyingDate (Date dt) = dt



dateFromString :: String -> Maybe Date
dateFromString str = Date <$> fromString str



defaultDate :: Partial => Date
defaultDate = Date (fromJust $ fromString "1982-01-01T05:00:00.000Z")



year :: Date -> D.Year
year = D.year <<< date <<< underlyingDate



month :: Date -> D.Month
month = D.month <<< date <<< underlyingDate



threeDecimalFix :: String -> Maybe String
threeDecimalFix s =
  case s' of
       [fst_half, snd_half] -> Just $ fst_half <> "." <> Str.take 3 snd_half <> "Z"
       _                    -> Nothing
  where
  s' = Str.split (Str.Pattern ".") s



threeDecimalCandidate :: String -> Maybe DateTime
threeDecimalCandidate s =
  case threeDecimalFix s of
       Nothing -> Nothing
       Just s' -> fromString s'



readDate :: Foreign -> F Date
readDate f =
  case tagOf f of
       "Date" ->
         case toDateTime <$> (runExcept $ unsafeReadTagged "Date" f) of
              Right (Just dt) -> pure $ Date dt
              Right Nothing   -> fail $ TypeMismatch "invalid date" "asdf"
              Left a          -> throwError a
       "String" -> do
         case (runExcept $ unsafeReadTagged "String" f) of
              Left a  -> throwError a
              Right s ->
                case fromString s of
                  Just dt -> pure $ Date dt
                  Nothing ->
                    -- weird scenario: Perhaps s is of the form: 2015-09-15T05:19:18.556641000000Z
                    -- everything (your phone, browser, etc) works fine.. except your kindle paperwhite.
                    -- for some reason this device wants: 2015-09-15T05:19:18.556 (3 decimal places)
                    -- that's what this weird piece of extra logic/parsing addresses:
                    -- https://en.wikipedia.org/wiki/ISO_8601#Times
                    -- hdgarrood is always a big help with date issues
                    case threeDecimalCandidate s of
                      Nothing -> fail $ TypeMismatch "invalid date" "invalid date"
                      Just dt -> pure $ Date dt
       "Number" ->
         case fromEpochMilliseconds <<< T.Milliseconds <$> runExcept (unsafeReadTagged "Number" f) of
              Right (Just dt) -> pure $ Date dt
              Right Nothing   -> fail (TypeMismatch "invalid read" "expecting epoch milliseconds")
              Left a          -> throwError a
       _ ->
         fail $ (TypeMismatch "Expecting date" (tagOf f))



yyyy_mm_dd :: Date -> String
yyyy_mm_dd dt =
  y <> "-" <> m <> "-" <> d
  where
    y = (ypad <<< show) case year dt of n -> n
    m = pad (1 + (fromEnum $ month dt))
    d = pad case dayOfMonth dt of DayOfMonth day -> day
    pad n = let str = show n
             in case Str.length str of
                   1 ->  "0" <> str
                   _ -> str
    ypad str =
      case Str.length str of
           0 -> "0000"
           1 -> "000" <> str
           2 -> "00" <> str
           3 -> "0" <> str
           _ -> str



failDate :: forall a. Either String a
failDate = Left "Could not parse Date."



-- | Attempts to construct a date from a string value using JavaScript’s
-- | [Date.parse() method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse).
-- | `Nothing` is returned if the parse fails or the resulting date is invalid.
fromString :: String -> Maybe DateTime
fromString = toDateTime <<< jsDateConstructor



-- | Effect type for when accessing the current date/time.
foreign import data Now :: Effect

-- | Gets a `Date` value for the current date/time according to the current
-- | machine’s local time.
now :: forall e. Partial => Eff (now :: Now | e) Date
now = nowImpl (\x -> Date $ fromJust $ toDateTime x)



-- | A day-of-month date component value.
newtype DayOfMonth = DayOfMonth Int

instance eqDayOfMonth :: Eq DayOfMonth where
  eq (DayOfMonth x) (DayOfMonth y) = x == y

instance ordDayOfMonth :: Ord DayOfMonth where
  compare (DayOfMonth x) (DayOfMonth y) = compare x y

instance showDayOfMonth :: Show DayOfMonth where
  show (DayOfMonth day) = "(DayOfMonth " <> show day <> ")"

-- | Gets the UTC day-of-month value for a date.
dayOfMonth :: Date -> DayOfMonth
dayOfMonth (Date d) = runFn2 jsDateMethod "getUTCDate" $ fromDateTime d




foreign import nowImpl :: forall e. (JSDate -> Date) -> Eff (now :: Now | e) Date

foreign import jsDateConstructor :: forall a. a -> JSDate

foreign import jsDateMethod :: forall a. Fn2 String JSDate a



-- | Converts a date value to an ISO 8601 Extended format date string.
toISOStringNoEff :: JSDate -> String
toISOStringNoEff dt = runFn2 jsDateMethod "toISOString" dt



fromEpochMilliseconds :: T.Milliseconds -> Maybe DateTime
fromEpochMilliseconds = toDateTime <<< jsDateConstructor
