module ReaderAPSpec where

import Test.Hspec
import Test.QuickCheck
import qualified ReaderAP as R
import qualified Data.ByteString as BS


spec :: Spec
spec =
  context "All readers of type ByteString -> Maybe (a,ByteString)."
    specInternal

specInternal :: Spec
specInternal = do
  describe "readNullAP" $ do
    context "when the head of a ByteString is 0x00" $ do
      it "returns ()" $
        R.bacRead R.nullAP (BS.singleton 0x00) `shouldBe` Just ((),BS.empty)

      it "consumes 1 byte (the head)" $
        property $ \ws ->
          let bs = BS.pack ws in
          R.bacRead R.nullAP (BS.cons 0x00 bs) `shouldBe` Just ((), bs)

    context "when the head of a ByteString is not 0x00" $
      it "fails" $
        property $
          forAll (choose (1,255))
            (\b -> R.bacRead R.nullAP (BS.singleton b) `shouldBe` Nothing)

    context "when the ByteString is empty" $
      it "fails" $
        R.bacRead R.nullAP BS.empty `shouldBe` Nothing

  describe "readBoolAP" $ do
    context "when the head is 0x10" $ do
      it "returns False" $
        R.bacRead R.boolAP (BS.singleton 0x10) `shouldBe` Just (False, BS.empty)

      it "consumes 1 byte" $
        property $ \ws ->
          let bs = BS.pack ws in
          R.bacRead R.boolAP (BS.cons 0x10 bs) `shouldBe` Just (False, bs)


    context "when the head is 0x11" $ do
      it "returns True" $
        R.bacRead R.boolAP (BS.singleton 0x11) `shouldBe` Just (True, BS.empty)

      it "consumes 1 byte" $
        property $ \ws ->
          let bs = BS.pack ws in
          R.bacRead R.boolAP (BS.cons 0x11 bs) `shouldBe` Just (True, bs)

    context "when the head is not 0x10 or 0x11" $
      it "fails" $
        property $
          forAll (choose (0,255) `suchThat` (\b -> b /= 0x10 && b /= 0x11))
          (\b -> R.bacRead R.boolAP (BS.singleton b) `shouldBe` Nothing)
