import           Stock.User
import           Test.Hspec

spec :: Spec
spec = do
  describe "user" $ do
    userCollection `shouldBe` "users"
