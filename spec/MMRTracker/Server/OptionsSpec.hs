module MMRTracker.Server.OptionsSpec where

import Test.Hspec
import MMRTracker.Server.Options

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  
  describe "constructConnectionString" do

    it "constructs a connection string" do
      let postgresOptions = PostgresOptions
            { postgresDatabaseName = "test"
            , postgresPort = Port 5432
            , postgresHost = Nothing
            , postgresUser = Nothing
            , postgresPassword = Nothing
            }
      constructConnectionString postgresOptions `shouldBe` "dbname=test port=5432"

    it "constructs a connection string with host, user, and password" do
      let postgresOptions = PostgresOptions
            { postgresDatabaseName = "test"
            , postgresPort = Port 5432
            , postgresHost = Just "localhost"
            , postgresUser = Just "testuser"
            , postgresPassword = Just "examplepassword"
            }
      constructConnectionString postgresOptions `shouldBe` "dbname=test port=5432 host=localhost user=testuser password=examplepassword"
