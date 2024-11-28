import Test.Hspec

main :: IO ()
main =
  hspec $
    describe "bluesky-stats-test" $
      it "works" $
        2 + 2 `shouldBe` (4 :: Int)
