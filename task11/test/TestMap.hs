{-# LANGUAGE ScopedTypeVariables #-}  -- Включаем некоторые расширения компилятора.
import Test.Tasty
import Test.Tasty.HUnit
import Data.Proxy
import Map
import qualified Data.Map.Strict as SMap
import MapInstance
import NaiveList(NaiveList)  -- Импортируем только тип NaiveList, но не его конструкторы Nil/Cons, чтобы не путались с конструкторами NaiveTree.
import NaiveTree

main :: IO ()
main = defaultMain testMap

{-|
  Генерирует группу тестов для конкретной реализации 'Map'
  с определённым именем.

  Мы хотим писать тесты один раз для всех возможных реализаций 'Map'.
  В чистом Haskell нам может помочь параметрический полиморфизм,
  но для этого нужно, чтобы в сигнатуре функции присутствовал
  тип из класса 'Map', который мы хотим протестировать.

  Специально для этих целей существует обёртка 'Data.Proxy', он
  позволяет передавать в функции даже типы высшего порядка.
-}
mapTests :: Map m => String -> Proxy m -> TestTree
mapTests name (_ :: Proxy m) =
    -- Чтобы можно было связать типовую переменную m здесь и в let ниже, нужно расширение ScopedTypeVariables.
    testGroup name [
        testGroup "Smoke tests" [
            testCase "toAscList . fromList sorts list" $
                let tr = fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                toAscList tr @?= [(1, "x"), (2, "a"), (3, "c")]
        ]
        ,
        testGroup "Tests for empty" [
            testCase "returns an empty map" $
                Map.null (empty :: m Int Int) @?= True
        ]
        ,
        testGroup "Tests for singleton" [
            testCase "returns a map of size 1 with the given (k, v) pair" $
                let mp :: m Int Int = singleton 1 2 in do
                    size mp @?= 1
                    Map.lookup 1 mp @?= Just 2
        ]
        ,
        testGroup "Tests for fromList" [
            testCase "empty list -> empty map" $
                Map.null (fromList [] :: m Int Int) @?= True
            ,
            testCase "single element list -> singleton" $
                let mp :: m Int Int = fromList [(1, 2)] in do
                    size mp @?= 1
                    Map.lookup 1 mp @?= Just 2
            ,
            testCase "toAscList . fromList sorts list" $
                let mp = fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                    toAscList mp @?= [(1, "x"), (2, "a"), (3, "c")]
        ]
        ,
        testGroup "Tests for toAscList" [
            testCase "toAscList . fromList sorts list" $
                let mp = fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                    toAscList mp @?= [(1, "x"), (2, "a"), (3, "c")]
        ]
        ,
        testGroup "Tests for insert" [
            testCase "works on empty map" $
                let mp = insert 1 2 (empty :: m Int Int) in do
                    size mp @?= 1
                    Map.lookup 1 mp @?= Just 2
            ,
            testCase "overwrites old value" $
                let mp = insert 1 2 (singleton 1 5 :: m Int Int) in
                    Map.lookup 1 mp @?= Just 2
        ]
        ,
        testGroup "Tests for insertWith" [
            testCase "works on empty map" $
                let mp = insertWith (-) 1 2 (empty :: m Int Int) in do
                    size mp @?= 1
                    Map.lookup 1 mp @?= Just 2
            ,
            testCase "stores f new_value old_value" $
                let mp = insertWith (-) 1 3 (singleton 1 2 :: m Int Int) in
                    Map.lookup 1 mp @?= Just 1
        ]
        ,
        testGroup "Tests for insertWithKey" [
            testCase "works on empty map" $
                let mp = insertWithKey (const . const) 1 2 (empty :: m Int Int) in do
                    size mp @?= 1
                    Map.lookup 1 mp @?= Just 2
            ,
            testCase "stores f key new_value old_value" $
                let mp = insertWithKey (\k v1 v2 -> (fst v1 + k, snd v2 - k))
                         1 (3, 8) (singleton 1 (10, 15) :: m Int (Int, Int)) in
                    Map.lookup 1 mp @?= Just (4, 14)
        ]
        ,
        testGroup "Tests for delete" [
            testCase "does nothing if key is not present" $
                let mp = delete 2 (singleton 1 5 :: m Int Int) in do
                    size mp @?= 1
                    Map.lookup 1 mp @?= Just 5
            ,
            testCase "removes key" $
                let mp = delete 1 (singleton 1 5 :: m Int Int) in
                    Map.null mp @?= True
        ]
        ,
        testGroup "Tests for adjust" [
            testCase "does nothing if key is not present" $
                let mp = adjust (+ 5) 2 (singleton 1 5 :: m Int Int) in do
                    size mp @?= 1
                    Map.lookup 1 mp @?= Just 5
            ,
            testCase "adjusts value with f" $
                let mp = adjust (+ 5) 1 (singleton 1 5 :: m Int Int) in do
                    size mp @?= 1
                    Map.lookup 1 mp @?= Just 10
        ]
        ,
        testGroup "Tests for adjustWithKey" [
            testCase "does nothing if key is not present" $
                let mp = adjustWithKey (-) 2 (singleton 1 5 :: m Int Int) in do
                    size mp @?= 1
                    Map.lookup 1 mp @?= Just 5
            ,
            testCase "adjusts value with f k" $
                let mp = adjustWithKey (-) 1 (singleton 1 5 :: m Int Int) in do
                    size mp @?= 1
                    Map.lookup 1 mp @?= Just (-4)
        ]
        ,
        testGroup "Tests for update" [
            testCase "does nothing if key is not present" $
                let mp = update pure 2 (singleton 1 5 :: m Int Int) in do
                    size mp @?= 1
                    Map.lookup 1 mp @?= Just 5
            ,
            testCase "can do conditional adjust/delete" $
                let f   = \a -> if a <= 30 then Just $ a + 10 else Nothing
                    mp  = fromList [(3, 25), (6, 50)] :: m Int Int 
                    mp' = update f 3 . update f 6 $ mp in do
                        size mp' @?= 1
                        Map.lookup 3 mp' @?= Just 35
        ]
        ,
        testGroup "Tests for updateWithKey" [
            testCase "does nothing if key is not present" $
                let mp = updateWithKey ((pure .) . (-)) 2 (singleton 1 5 :: m Int Int) in do
                    size mp @?= 1
                    Map.lookup 1 mp @?= Just 5
            ,
            testCase "can do conditional adjust/delete" $
                let f   = \k a -> if a <= 30 then Just $ k * a + 10 else Nothing
                    mp  = fromList [(3, 25), (6, 50)] :: m Int Int 
                    mp' = updateWithKey f 3 . updateWithKey f 6 $ mp in do
                        Map.lookup 6 mp' @?= Nothing
                        Map.lookup 3 mp' @?= Just 85
        ]
        ,
        testGroup "Tests for alter" [
            testCase "can add and remove values conditionally" $
                let f   = \ma -> case ma of
                                   Just a  -> if a <= 30 then Just $ a + 10 else Nothing 
                                   Nothing -> Just 100
                    mp  = fromList [(3, 25), (6, 50)] :: m Int Int
                    mp' = alter f 3 . alter f 6 . alter f 10 $ mp in do
                        Map.lookup 6 mp' @?= Nothing
                        Map.lookup 3 mp' @?= Just 35
                        Map.lookup 10 mp' @?= Just 100
        ]
        ,
        testGroup "Tests for lookup" [
            testCase "returns Nothing if key is not present" $
                let mp = fromList [(3, 25), (6, 50)] :: m Int Int in
                    Map.lookup 1 mp @?= Nothing
            ,
            testCase "returns found value" $
                let mp = fromList [(3, 25), (6, 50)] :: m Int Int in
                    Map.lookup 3 mp @?= Just 25
        ]
        ,
        testGroup "Tests for member" [
            testCase "returns True if key is present" $
                let mp = fromList [(3, 25), (6, 50)] :: m Int Int in
                    member 3 mp @?= True
            ,
            testCase "returns False if key is not present" $
                let mp = fromList [(3, 25), (6, 50)] :: m Int Int in
                    member 5 mp @?= False
        ]
        ,
        testGroup "Tests for notMember" [
            testCase "returns False if key is present" $
                let mp = fromList [(3, 25), (6, 50)] :: m Int Int in
                    notMember 3 mp @?= False
            ,
            testCase "returns True if key is not present" $
                let mp = fromList [(3, 25), (6, 50)] :: m Int Int in
                    notMember 5 mp @?= True
        ]
        ,
        testGroup "Tests for null" [
            testCase "returns True for empty maps" $
                let mp = empty :: m Int Int in
                    Map.null mp @?= True
            ,
            testCase "returns False for non-empty maps" $
                let mp = fromList [(3, 25), (6, 50)] :: m Int Int in
                    Map.null mp @?= False
        ]
        ,
        testGroup "Tests for size" [
            testCase "returns size of the map" $
                let mp = fromList [(3, 25), (6, 50)] :: m Int Int in
                    size mp @?= 2
        ]
    ]

testNaiveTree :: TestTree
testNaiveTree = testGroup "Test NaiveTree" [
        testGroup "merge" [
            testCase "merge empty" $
                merge Nil Nil @?= (Nil :: NaiveTree () ())
            ,
            testCase "merge two nodes" $
                -- Ваша реализация может выдавать другое дерево, соответствующее
                -- последовательности 1, 2.
                merge (Node 1 "a" Nil Nil) (Node 2 "b" Nil Nil)
                    @?= Node 1 "a" Nil (Node 2 "b" Nil Nil)
        ]
    ]

testMap :: TestTree
testMap = testGroup "Testing implementations of trees"
    [
        mapTests "Data.Map.Strict" (Proxy :: Proxy SMap.Map),
        mapTests "NaiveList" (Proxy :: Proxy NaiveList),
        mapTests "NaiveTree" (Proxy :: Proxy NaiveTree),
        testNaiveTree
    ]
