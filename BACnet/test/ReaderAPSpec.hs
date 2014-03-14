module ReaderAPSpec where

import Test.Hspec
import Test.QuickCheck
import qualified ReaderAP as R
import qualified Data.ByteString as BS


spec :: Spec
spec = do
  context "All readers of type ByteString -> Maybe (a,ByteString)."
    specInternal

specInternal :: Spec
specInternal = do
  describe "readNullAP" $ do
    context "when the head of a ByteString is 0x00" $ do
      it "returns success ()" $ do
        R.bacRead R.nullAP (BS.singleton 0x00) `shouldBe` Just ((),BS.empty)

      it "consumes 1 byte (the head)" $ do
        property $ \ws -> 
          let bs = BS.pack ws in
          R.bacRead R.nullAP (BS.cons 0x00 bs) `shouldBe` Just ((), bs)

    context "when the head of a ByteString is not 0x00" $ do
      it "returns failure" $ do
        property $ 
          forAll (choose (1,255)) 
            (\b -> R.bacRead R.nullAP (BS.singleton b) `shouldBe` Nothing)

{-
    context "when the ByteString is empty" $ do
      it "returns failure" $ do
        R.readit R.readNullAP (BS.empty) `shouldBe` Nothing

  describe "readNullCS" $ do
    context "when the head of a ByteString is 0x89" $ do
      it "returns failure" $ do
        R.readit (R.readNullCS 8) (BS.singleton 0x89) `shouldBe` Nothing

    it "reads 0x08" $ do
      R.readit (R.readNullCS 0) (BS.singleton 0x08) `shouldBe` Just((),BS.empty)

-}
