module DarkPlaces.TextSpec (
    spec
) where
import Test.Hspec
import DarkPlaces.Text
import DarkPlaces.Text.Types
import Test.QuickCheck
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Char
import Data.Monoid


instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary


instance (Arbitrary a) => Arbitrary (DPTextToken a) where
    arbitrary = do
        t <- choose (0, 3) :: Gen Int
        case t of
            0 -> SimpleColor <$> choose (0, 9)
            1 -> HexColor <$> choose (0, 0xFFF)
            2 -> DPString <$> arbitrary
            3 -> return DPNewline


instance (Arbitrary a) => Arbitrary (DPText a) where
    arbitrary = DPText <$> arbitrary


spec :: Spec
spec = do
    describe "parseDPText" $ do
        it "test base examples" $ do
            let res = DPText [
                      SimpleColor 1,DPString "one",SimpleColor 2,
                      DPString "two",HexColor 4095,DPString "three",
                      DPNewline,DPString "four"]

            parseDPText "^1one^2two^xfffthree\nfour" `shouldBe` res

        it "parsing string and converting back to string give same str" $ do
            property $ \xs -> lower (toText $ parseDPText xs) == lower xs

        it "converting to string and parsing gives same result" $ property $
            \xs -> toText (parseDPText $ toText xs) == toText xs

        it "check monoid property" $ property $
            \f s -> toText (f <> s) == (toText f :: BL.ByteString) <> (toText s)

    describe "stripColors" $ do
        it "should remove colors tokens" $ property $
            \xs -> (\(DPText s) -> not $ any isColor s) (stripColors xs :: DPText BL.ByteString)
  where
    lower = BLC.map toLower
