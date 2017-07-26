module Test.Main where



import Control.Monad.Aff.AVar           (AVAR())
import Control.Monad.Eff                (Eff())
import Control.Monad.Eff.Console        (CONSOLE())
import Data.Argonaut.Core               (jsonEmptyObject)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Argonaut.Decode             (class DecodeJson, decodeJson)
import Data.Argonaut.Encode             (class EncodeJson, encodeJson)
import Data.Date.Helpers                (Date, dateFromString, defaultDate, threeDecimalFix)
import Data.Either                      (Either, either)
import Data.Maybe                       (Maybe (..))
import Prelude                          (class Show, show, class Eq, id, Unit, const, pure, bind, discard, ($), (==), (&&))
import Test.Unit                        (test, suite)
import Test.Unit.Assert                 as Assert
import Test.Unit.Console                (TESTOUTPUT())
import Test.Unit.Main                   (runTest)



newtype Test1 = Test1 {
  f1 :: Boolean,
  d1 :: Date
}

instance test1Eq :: Eq Test1 where
  eq (Test1 a) (Test1 b) = a.f1 == b.f1 && a.d1 == b.d1

instance test1Show :: Show Test1 where
  show _ = "Test1"

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



main :: forall eff. Eff (avar :: AVAR, console :: CONSOLE, testOutput :: TESTOUTPUT | eff) Unit
main = runTest do

  suite "purescript-date-helpers" do

    test "Internal Tests" do

      Assert.assertFalse "Should not be equal." $ test1_0 == test1_a
      Assert.equal test1_a test1_a

      Assert.assertFalse "Should not be equal." $ test2_0 == test2_a
      Assert.equal test2_a test2_a



    test "Argonaut Date Helpers" do

      Assert.equal
        "2017-01-01T05:00:00.000Z"
        $ either id show (decodeJson (encodeJson (dateFromString "2017-01-01T05:00:00.000Z")) :: Either String Date)

      Assert.equal
        "2016-08-19T22:47:43.886Z"
        $ either id show (decodeJson (encodeJson (dateFromString "2016-08-19T22:47:43.886804Z")) :: Either String Date)

      Assert.equal
        test1_a
        $ either (const test1_0) id (decodeJson (encodeJson test1_a) :: Either String Test1)

      Assert.equal
        test2_a
        $ either (const test2_0) id (decodeJson (encodeJson test2_a) :: Either String Test2)

      Assert.equal
        test2_0
        $ either (const test2_a) id (decodeJson (encodeJson test2_0) :: Either String Test2)



    test "Weird case: threeDecimalCandidate" do
      Assert.equal
        (Just "2015-09-15T05:19:18.556Z")
        $ threeDecimalFix "2015-09-15T05:19:18.556641000000Z"

      Assert.equal
        "2015-09-15T05:19:18.556Z"
        $ either id show (decodeJson (encodeJson (dateFromString "2015-09-15T05:19:18.556641000000Z")) :: Either String Date)


    where
    test1_0 = Test1 {f1: false, d1: defaultDate}
    test1_a = Test1 {f1: true,  d1: defaultDate}
    test2_0 = Test2 {f2: false, d2: Nothing}
    test2_a = Test2 {f2: true,  d2: Just defaultDate}
