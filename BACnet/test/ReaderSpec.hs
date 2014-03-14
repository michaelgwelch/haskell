module ReaderSpec where

import Test.Hspec
import Test.QuickCheck
import qualified Reader as R
import qualified Data.ByteString as BS


spec :: Spec
spec = do
  context "All readers take a ByteString and return Maybe (a,ByteString)."
    specInternal

specInternal :: Spec
specInternal = do
  describe "readNullAP" $ do
    context "when the head of a ByteString is 0x00" $ do
      it "returns success ()" $ do
        R.readit R.readNullAP (BS.singleton 0x00) `shouldBe` Just ((),BS.empty)

      it "consumes 1 byte (the head)" $ do
        property $ \ws -> 
          let bs = BS.pack ws in
          R.readit R.readNullAP (BS.cons 0x00 bs) `shouldBe` Just((), bs)

    context "when the head of a ByteString is not 0x00" $ do
      it "returns failure" $ do
        property $ 
          forAll (choose (1,255)) 
            (\b -> R.readit R.readNullAP (BS.singleton b) `shouldBe` Nothing)

    context "when the ByteString is empty" $ do
      it "returns failure" $ do
        R.readit R.readNullAP (BS.empty) `shouldBe` Nothing


  describe "readNullCS" $ do
    context "when the head of a ByteString is 0x89" $ do
      it "returns failure" $ do
        R.readit (R.readNullCS 8) (BS.singleton 0x89) `shouldBe` Nothing

    it "reads 0x08" $ do
      R.readit (R.readNullCS 0) (BS.singleton 0x08) `shouldBe` Just((),BS.empty)

