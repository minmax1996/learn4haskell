{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# LANGUAGE TypeApplications #-}

module Test.Chapter4
    ( chapter4
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Chapter4


chapter4 :: Spec
chapter4 = describe "Chapter4" $ do
    chapter4normal
    chapter4advanced
    chapter4tree

chapter4normal :: Spec
chapter4normal = describe "Chapter4Normal" $ do
    describe "Task2: Functor for Secret" $ do
        let trap = Trap "it's a trap"
        it "doen't affect trap" $
            fmap @(Secret String) @Bool not trap `shouldBe` trap
        it "change reward, same type" $
            fmap @(Secret String) @Bool not (Reward False) `shouldBe` Reward True
        it "change reward, other type" $
            fmap @(Secret String) @Int even (Reward 5) `shouldBe` Reward False
        it "change reward, other type" $
            fmap @(Secret String) @Int even (Reward 4) `shouldBe` Reward True
    describe "Task4: Applicative for Secret" $ do
        let trap :: Secret String Int
            trap = Trap "it's a trap"
        it "pure int" $
            pure @(Secret String) "x" `shouldBe` Reward "x"
        it "pure bool" $
            pure @(Secret String) False `shouldBe` Reward False
        it "trap <*> reward" $
            Trap "it's a trap" <*> Reward 42 `shouldBe` trap
        it "trap <*> trap" $
            Trap "it's a trap" <*> Trap "42" `shouldBe` trap
        it "reward <*> trap" $
            Reward not <*> Trap 42 `shouldBe` Trap 42
        it "reward <*> reward - same type" $
            Reward not <*> Reward True `shouldBe` (Reward False :: Secret String Bool)
        it "reward <*> reward" $
            Reward odd <*> Reward 42 `shouldBe` (Reward False :: Secret String Bool)
    describe "Task6: Monad for Secret" $ do
        it "Trap" $ (Trap "aaar" >>= halfSecret) `shouldBe` Trap "aaar"
        it "Reward even" $ (Reward 42 >>= halfSecret) `shouldBe` Reward 21
        it "Reward odd" $ (Reward 11 >>= halfSecret) `shouldBe` Trap "it's a trap"

chapter4advanced :: Spec
chapter4advanced = describe "Chapter4Advanced" $
    describe "andM" $ do
        it "Nothing - Nothing" $ andM Nothing Nothing `shouldBe` Nothing
        it "Nothing - Just" $ andM Nothing (Just True) `shouldBe` Nothing
        it "Just True - Nothing" $ andM (Just True) Nothing `shouldBe` Nothing
        it "Just False - Nothing" $ andM (Just False) Nothing `shouldBe` Just False
        it "Just - Just : False" $ andM (Just True) (Just False) `shouldBe` Just False
        it "Just - Just : True" $ andM (Just True) (Just True) `shouldBe` Just True

chapter4tree :: Spec
chapter4tree = describe "chapter4tree" $
    describe "appendToBST" $ do
        it "append To root" $ appendBST 0 NoValue `shouldBe` Node 0 NoValue NoValue
        it "append TO Same" $ appendBST 0 (leaf 0) `shouldBe` Node 0 NoValue NoValue
        it "append To right subTree" $ appendBST 3 (leaf 0) `shouldBe` Node 0 NoValue (Node 3 NoValue NoValue)
        it "append To left subTree" $ appendBST (-3) (leaf 0) `shouldBe` Node 0 (Node (-3) NoValue NoValue) NoValue
        it "mk Empty" $ mkBinarySearchTree ([] :: [Int]) `shouldBe` NoValue
        it "mk single Element" $ mkBinarySearchTree [0] `shouldBe` Node 0 NoValue NoValue
        it "mk one subtree" $ mkBinarySearchTree [1,2,3,4,5] `shouldBe` Node 5 (Node 4 (Node 3 (Node 2 (Node 1 NoValue NoValue) NoValue) NoValue) NoValue) NoValue
        it "mk Hello World" $ mkBinarySearchTree "HelloWorld" `shouldBe` Node 'd' (Node 'W' (Node 'H' NoValue NoValue) NoValue) (Node 'l' (Node 'e' NoValue NoValue) (Node 'r' (Node 'o' NoValue NoValue) NoValue))
        it "fmap (+1)" $ fmap (+1) (mkBinarySearchTree [1,2,3,4,5]) `shouldBe` Node 6 (Node 5 (Node 4 (Node 3 (Node 2 NoValue NoValue) NoValue) NoValue) NoValue) NoValue
        it "fmap (not) to Tree of Bools" $ fmap (not) (Node True (leaf False) (leaf False)) `shouldBe` Node False (Node True NoValue NoValue) (Node True NoValue NoValue)
        it "aplicative bools" $ leaf not <*> leaf True `shouldBe` Node False NoValue NoValue
        it "aplicative ints" $ leaf (+1) <*> leaf 1 `shouldBe` Node 2 NoValue NoValue
        it "aplicative deeper" $ Node (+1) (leaf (+2)) (leaf (+3)) <*> mkBinarySearchTree [3,1,2] `shouldBe` Node 3 (Node 3 NoValue NoValue) (Node 6 NoValue NoValue)
        it "makeRangeTree" $ makeRangeTree 6 `shouldBe` Node 6 (Node 5 (Node 4 (Node 3 (Node 2 (Node 1 NoValue NoValue) NoValue) NoValue) NoValue) NoValue) NoValue
        it "monads for leaf" $ (leaf 6 >>= makeRangeTree) `shouldBe` Node 6 (Node 4 NoValue (Node 2 NoValue NoValue)) (Node 5 (Node 1 NoValue NoValue) (Node 3 NoValue NoValue))
        it "monads for more" $ (mkBinarySearchTree [2,1,5,3] >>= makeRangeTree) `shouldBe` Node 5 (Node 3 (Node 2 NoValue (Node 1 NoValue NoValue)) (Node 1 NoValue (Node 2 NoValue NoValue))) (Node 4 (Node 3 NoValue (Node 1 NoValue NoValue)) (Node 2 NoValue (Node 1 NoValue NoValue)))
        it "monads for more" $ (mkBinarySearchTree [2,1,5,3] >>= makeRangeTree) `shouldBe` Node 5 (Node 3 (Node 2 NoValue (Node 1 NoValue NoValue)) (Node 1 NoValue (Node 2 NoValue NoValue))) (Node 4 (Node 3 NoValue (Node 1 NoValue NoValue)) (Node 2 NoValue (Node 1 NoValue NoValue)))
        it "reverse Tree" $ reverseTree (mkBinarySearchTree "HelloWorld") `shouldBe` Node 'd' (Node 'l' (Node 'r' NoValue (Node 'o' NoValue NoValue)) (Node 'e' NoValue NoValue)) (Node 'W' NoValue (Node 'H' NoValue NoValue))
        it "reverse reverse Tree" $ (reverseTree $ reverseTree $ (mkBinarySearchTree "HelloWorld")) `shouldBe` (mkBinarySearchTree "HelloWorld")
        it "TreeToList" $ treeToList (mkBinarySearchTree "HelloWorld") `shouldBe` Cons 'H' (Cons 'W' (Cons 'd' (Cons 'e' (Cons 'l' (Cons 'o' (Cons 'r' Empty))))))
        it "reverseTreeToList" $ (treeToList $ reverseTree $ mkBinarySearchTree "HelloWorld") `shouldBe` Cons 'r' (Cons 'o' (Cons 'l' (Cons 'e' (Cons 'd' (Cons 'W' (Cons 'H' Empty))))))



halfSecret :: Int -> Secret String Int
halfSecret n
    | even n = Reward (div n 2)
    | otherwise = Trap "it's a trap"
