module DarkPlaces.TextSpec (
    spec
) where
import Test.Hspec
import DarkPlaces.Text


spec :: Spec
spec = do
    describe "parseDPText" $ do
        it "test base examples" $ do
            let res = DPText [
                      SimpleColor 1,DPString "one",SimpleColor 2,
                      DPString "two",HexColor 4095,DPString "three",
                      DPNewline,DPString "four"]

            parseDPText "^1one^2two^xfffthree\nfour" `shouldBe` res
