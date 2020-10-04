module Test.Chapter3
    ( chapter3
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Chapter3


chapter3 :: Spec
chapter3 = describe "Chapter3" $ do
    describe "Chapter3Normal" $ it "" $ True `shouldBe` True
    describe "Chapter3Advanced" $ it "" $ True `shouldBe` True
    describe "Task2: fight beetween knight and moster" $ do
        it "in fight knight wins" $ fight (MkMonster 10 5 5) (MkKnight 10 15 14) `shouldBe` 19
        it "in fight monster wins" $ fight (MkMonster 10 5 5) (MkKnight 4 5 14) `shouldBe` -1
        it "in fight nobody wins" $ fight (MkMonster 10 5 5) (MkKnight 10 5 5) `shouldBe` 5
    describe "Task4: magical city" $ do
        it "in build with no castle" $ buildCastle "NewName" (MkCity NoCastle NoWall Church []) `shouldBe` (MkCity (Castle "NewName") NoWall Church [])
        it "in build castle with Castle" $ buildCastle "NewName" (MkCity (Castle "OldOne") NoWall Church []) `shouldBe` (MkCity (Castle "NewName") NoWall Church [])
        it "in build House with no houses in city" $ buildHouse 1 (MkCity NoCastle NoWall Church []) `shouldBe` (MkCity NoCastle NoWall Church [MkHouse 1])
        it "in build House with one house in city" $ buildHouse 2 (MkCity NoCastle NoWall Church [MkHouse 1]) `shouldBe` (MkCity NoCastle NoWall Church [MkHouse 2, MkHouse 1])
        it "in build House with more 4 peopel" $ buildHouse 5 (MkCity NoCastle NoWall Church [MkHouse 1]) `shouldBe` (MkCity NoCastle NoWall Church [MkHouse 1])
        it "in build wall with no castle" $ buildWalls (MkCity NoCastle NoWall Church [MkHouse 1]) `shouldBe` (MkCity NoCastle NoWall Church [MkHouse 1])
        it "in build wall with Wall" $ buildWalls (MkCity NoCastle Wall Church [MkHouse 1]) `shouldBe` (MkCity NoCastle Wall Church [MkHouse 1])
        it "count people" $ countPeople [MkHouse 1,MkHouse 2,MkHouse 4] `shouldBe` 7
        it "count people with no houses" $ countPeople [] `shouldBe` 0
        it "in build wall with Castle and no people" $ buildWalls (MkCity (Castle "New") NoWall Church [MkHouse 1]) `shouldBe` (MkCity (Castle "New") NoWall Church [MkHouse 1])
        it "in build wall with Castle and People" $ buildWalls (MkCity (Castle "New") NoWall Church [MkHouse 4,MkHouse 4,MkHouse 4]) `shouldBe` (MkCity (Castle "New") Wall Church [MkHouse 4,MkHouse 4,MkHouse 4])
    describe "Task7: Appends" $ do
        it "append Golds" $ append (Gold 10) (Gold 20) `shouldBe` (Gold 30)
        it "append Lists" $ append ([1,2,3] :: [Int]) ([4,5,6] :: [Int]) `shouldBe` [1,2,3,4,5,6]
        it "append Maybees" $ append  (Just ([1,2,3] :: [Int])) (Just ([4,5,6] :: [Int])) `shouldBe` Just [1,2,3,4,5,6]
        it "append Just1" $ append  (Just ([1,2,3] :: [Int])) Nothing `shouldBe` Just [1,2,3]
        it "append Just2" $ append  (Nothing) (Just ([4,5,6] :: [Int])) `shouldBe` Just [4,5,6]
        --it "append Nothings" $ append (Nothing) (Nothing) `shouldBe` Nothing
    describe "Task8: Enums" $ do
        it "nextDay in Middle1" $ nextDay Monday `shouldBe` Tuesday
        it "nextDay in Middle1" $ nextDay Friday `shouldBe` Saturday
        it "nextDay in Sunday" $ nextDay Sunday `shouldBe` Monday
        it "is Weekend Sunday" $ isWeekend Sunday `shouldBe` True
        it "is Weekend Saturday" $ isWeekend Saturday `shouldBe` True
        it "is Weekend Saturday" $ isWeekend Monday `shouldBe` False
        it "days to next party" $ daysToParty Friday `shouldBe` 7
        it "days to party in Monday" $ daysToParty Monday `shouldBe` 4
        it "days to next party day after party " $ daysToParty Saturday `shouldBe` 6

