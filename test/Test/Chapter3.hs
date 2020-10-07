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
        it "in build with no castle" $ buildCastle "NewName" (MkCity NoCastle Church []) `shouldBe` (MkCity (Castle "NewName") Church [])
        it "in build castle with Castle" $ buildCastle "NewName" (MkCity (Castle "OldOne") Church []) `shouldBe` (MkCity (Castle "NewName") Church [])
        it "in build House with no houses in city" $ buildHouse 1 (MkCity NoCastle Church []) `shouldBe` (MkCity NoCastle Church [House 1])
        it "in build House with one house in city" $ buildHouse 2 (MkCity NoCastle Church [House 1]) `shouldBe` (MkCity NoCastle Church [House 2, House 1])
        it "in build House with more 4 peopel" $ buildHouse 5 (MkCity NoCastle Church [House 1]) `shouldBe` (MkCity NoCastle Church [House 1])
        it "in build wall with no castle" $ buildWalls (MkCity NoCastle Church [House 1]) `shouldBe` (MkCity NoCastle Church [House 1])
        it "in build wall with Wall" $ buildWalls (MkCity NoCastle Church [House 1]) `shouldBe` (MkCity NoCastle Church [House 1])
        it "count people" $ countPeople [House 1,House 2,House 4] `shouldBe` 7
        it "count people with no houses" $ countPeople [] `shouldBe` 0
        it "in build wall with Castle and no people" $ buildWalls (MkCity (Castle "New") Church [House 1]) `shouldBe` (MkCity (Castle "New") Church [House 1])
        it "in build wall with Castle and People" $ buildWalls (MkCity (Castle "New") Church [House 4,House 4,House 4]) `shouldBe` (MkCity (CastleWithWall "New") Church [House 4,House 4,House 4])
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
    describe "Task9: Fight" $ do
        it "Knight can drink potion" $ drinkPotion (KnightPlayer (Health 5) (Attack 5) (Defence 5) True) `shouldBe` KnightPlayer (Health 10) (Attack 5) (Defence 5) True
        it "Knight can cast spell" $ castSpell (KnightPlayer (Health 5) (Attack 5) (Defence 5) True) `shouldBe` KnightPlayer (Health 5) (Attack 5) (Defence 10) True
        it "Knight can drink and cast spell" $ (drinkPotion $ castSpell (KnightPlayer (Health 5) (Attack 5) (Defence 5) True)) `shouldBe` KnightPlayer (Health 10) (Attack 5) (Defence 10) True
        it "Knight can drink and cast spell" $ (drinkPotion $ castSpell (KnightPlayer (Health 5) (Attack 5) (Defence 5) True)) `shouldBe` KnightPlayer (Health 10) (Attack 5) (Defence 10) True
        it "Monster can run away" $ runAway (MonsterPlayer (Health 5) (Attack 5) True) `shouldBe` MonsterPlayer (Health 5) (Attack 5) False
        it "Knight can hit Knight" $ knightHit (KnightPlayer (Health 5) (Attack 5) (Defence 5) True) (KnightPlayer (Health 10) (Attack 5) (Defence 1) True) `shouldBe` (KnightPlayer (Health 6) (Attack 5) (Defence 1) True)
        it "Knight can kill Knight" $ knightHit (KnightPlayer (Health 5) (Attack 55) (Defence 5) True) (KnightPlayer (Health 10) (Attack 5) (Defence 1) True) `shouldBe` (KnightPlayer (Health (-44)) (Attack 5) (Defence 1) False)
        it "Knight can hit Monster" $ knightHit (KnightPlayer (Health 5) (Attack 5) (Defence 5) True) (MonsterPlayer (Health 10) (Attack 5) True) `shouldBe` (MonsterPlayer (Health 5) (Attack 5) True)
        it "Knight can kill Monster"  $ knightHit (KnightPlayer (Health 5) (Attack 55) (Defence 5) True) (MonsterPlayer (Health 10) (Attack 5) True) `shouldBe` (MonsterPlayer (Health (-45)) (Attack 5) False)
        it "Monster can hit Knight"   $ monsterHit (MonsterPlayer (Health 5) (Attack 5)  True) (KnightPlayer (Health 10) (Attack 5) (Defence 1) True) `shouldBe` (KnightPlayer (Health 6) (Attack 5) (Defence 1) True)
        it "Monster can kill Knight"  $ monsterHit (MonsterPlayer (Health 5) (Attack 55) True) (KnightPlayer (Health 10) (Attack 5) (Defence 1) True) `shouldBe` (KnightPlayer (Health (-44)) (Attack 5) (Defence 1) False)
        it "Monster can hit Monster"  $ monsterHit (MonsterPlayer (Health 5) (Attack 5)  True) (MonsterPlayer (Health 10) (Attack 5) True) `shouldBe` (MonsterPlayer (Health 5) (Attack 5) True)
        it "Monster can kill Monster" $ monsterHit (MonsterPlayer (Health 5) (Attack 55) True) (MonsterPlayer (Health 10) (Attack 5) True) `shouldBe` (MonsterPlayer (Health (-45)) (Attack 5) False)
        it "Knight that can kill other Knight not kills him" $ knightHit (KnightPlayer (Health 5) (Attack 15) (Defence 5) True) (drinkPotion $ KnightPlayer (Health 10) (Attack 5) (Defence 5) True) `shouldBe` (KnightPlayer (Health 5) (Attack 5) (Defence 5) True)
        it "Battle(K-M)WithOneRound(KWin)"  $ battle (mkKnightPlayer 10 10 1) (mkMonsterPlayer 10 3)   `shouldBe` "FirstWin"
        it "Battle(K-M)WithOneRound(MWin)"  $ battle (mkKnightPlayer 10 5 1)  (mkMonsterPlayer 10 13)  `shouldBe` "SecondWin"
        it "Battle(M-K)WithOneRound(KWin)"  $ battle (mkMonsterPlayer 10 3)   (mkKnightPlayer 10 15 1) `shouldBe` "SecondWin"
        it "Battle(M-K)WithOneRound(MWin)"  $ battle (mkMonsterPlayer 10 20)  (mkKnightPlayer 10 15 1) `shouldBe` "FirstWin"
        it "Battle(K-K)WithOneRound(K1Win)" $ battle (mkKnightPlayer 10 15 1) (mkKnightPlayer 10 13 1) `shouldBe` "FirstWin"
        it "Battle(K-K)WithOneRound(K2Win)" $ battle (mkKnightPlayer 10 15 1) (mkKnightPlayer 10 13 6) `shouldBe` "SecondWin"
        it "Battle(M-M)WithOneRound(M1Win)" $ battle (mkMonsterPlayer 10 15)  (mkMonsterPlayer 10 13)  `shouldBe` "FirstWin"
        it "Battle(M-M)WithOneRound(M2Win)" $ battle (mkMonsterPlayer 10 15)  (mkMonsterPlayer 20 13)  `shouldBe` "SecondWin"
        it "Battle(K-M)MultiRounds(M2Win)"  $ battle (mkKnightPlayer 10 5 1) (mkMonsterPlayer 10 3)    `shouldBe` "FirstWin"
        it "Battle(K-M)MultiRounds(M2Win)"  $ battle (mkKnightPlayer 50 5 1) (mkMonsterPlayer 50 3)    `shouldBe` "FirstWin"

