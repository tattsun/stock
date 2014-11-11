import           Stock.User
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "user" $ do
    userCollection `shouldBe` "users"
