module Test.Main where



import Prelude                   (class Show, class Eq, id, const, ($), bind, (==), pure, (&&))
import Data.Argonaut.Core        (jsonEmptyObject)
import Data.Argonaut.Combinators ((.?), (~>), (:=))
import Data.Argonaut.Decode      (class DecodeJson, decodeJson)
import Data.Argonaut.Encode      (class EncodeJson, encodeJson)
import Data.Date.Helpers         (Date, dateFromString, toISOString, defaultDate)
import Data.Either               (Either, either)
import Data.JSON                 (class FromJSON, class ToJSON, JValue(JObject), toJSON
                                 , parseJSON, fail, (.:), (.=), object)
import Data.Maybe                (Maybe (..))
import Test.Unit                 (test, runTest)
import Test.Unit.Assert          as Assert



newtype Test1 = Test1 {
  f1 :: Boolean,
  d1 :: Date
}

instance test1Eq :: Eq Test1 where
  eq (Test1 a) (Test1 b) = a.f1 == b.f1 && a.d1 == b.d1

instance test1Show :: Show Test1 where
  show _ = "Test1"

instance test1ToJSON :: ToJSON Test1 where
  toJSON (Test1 t) = object $
    [ "f1" .= t.f1
    , "d1" .= t.d1
    ]

instance test1FromJSON :: FromJSON Test1 where
  parseJSON (JObject o) = do
    f1 <- o .: "f1"
    d1 <- o .: "d1"
    pure $ Test1 {f1, d1}
  parseJSON _ = fail "Unable to parse object"

instance test1EncodeJson :: EncodeJson Test1 where
  encodeJson (Test1 t) =
       "f1" := t.f1
    ~> "d1" := t.d1
    ~> jsonEmptyObject

instance test1DecodeJson :: DecodeJson Test1 where
  decodeJson json = do
    obj <- decodeJson json
    f1 <- obj .? "f1"
    d1 <- obj .? "d1"
    pure $ Test1 {f1, d1}



newtype Test2 = Test2 {
  f2 :: Boolean,
  d2 :: Maybe Date
}

instance test2Eq :: Eq Test2 where
  eq (Test2 a) (Test2 b) = a.f2 == b.f2 && a.d2 == b.d2

instance test2Show :: Show Test2 where
  show _ = "Test2"

instance test2ToJSON :: ToJSON Test2 where
  toJSON (Test2 t) = object $
    [ "f2" .= t.f2
    , "d2" .= t.d2
    ]

instance test2FromJSON :: FromJSON Test2 where
  parseJSON (JObject o) = do
    f2 <- o .: "f2"
    d2 <- o .: "d2"
    pure $ Test2 {f2, d2}
  parseJSON _ = fail "Unable to parse object"

instance test2EncodeJson :: EncodeJson Test2 where
  encodeJson (Test2 t) =
       "f2" := t.f2
    ~> "d2" := t.d2
    ~> jsonEmptyObject

instance test2DecodeJson :: DecodeJson Test2 where
  decodeJson json = do
    obj <- decodeJson json
    f2 <- obj .? "f2"
    d2 <- obj .? "d2"
    pure $ Test2 {f2, d2}



main = runTest do

  test "Internal Tests" do

    Assert.assertFalse "Should not be equal." $ test1_0 == test1_a
    Assert.equal test1_a test1_a

    Assert.assertFalse "Should not be equal." $ test2_0 == test2_a
    Assert.equal test2_a test2_a


  test "Argonaut Date Helpers" do

    Assert.equal
      "2017-01-01T05:00:00.000Z"
      $ either id toISOString (decodeJson (encodeJson (dateFromString "2017-01-01T05:00:00.000Z")) :: Either String Date)

    Assert.equal
      test1_a
      $ either (const test1_0) id (decodeJson (encodeJson test1_a) :: Either String Test1)

    Assert.equal
      test2_a
      $ either (const test2_0) id (decodeJson (encodeJson test2_a) :: Either String Test2)

    Assert.equal
      test2_0
      $ either (const test2_a) id (decodeJson (encodeJson test2_0) :: Either String Test2)


  test "JSON Date Helpers" do
    Assert.equal
      "2017-01-01T05:00:00.000Z"
      $ either id toISOString (parseJSON (toJSON (dateFromString "2017-01-01T05:00:00.000Z")) :: Either String Date)

    Assert.equal
      test1_a
      $ either (const test1_0) id (parseJSON (toJSON test1_a) :: Either String Test1)

    Assert.equal
      test2_a
      $ either (const test2_0) id (parseJSON (toJSON test2_a) :: Either String Test2)

    Assert.equal
      test2_0
      $ either (const test2_a) id (parseJSON (toJSON test2_0) :: Either String Test2)


  where
  test1_0 = Test1 {f1: false, d1: defaultDate}
  test1_a = Test1 {f1: true,  d1: defaultDate}
  test2_0 = Test2 {f2: false, d2: Nothing}
  test2_a = Test2 {f2: true,  d2: Just defaultDate}
