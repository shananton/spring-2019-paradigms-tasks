import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 50 50
        bob = robot "Bob" 50 100
        scrap = robot "Boop" 777 0
        weak = robot "Weaky" 1 1
        dummy = robot "Dummy" 1 120
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for getName" $
            getName walter @?= "Walter"
        
        , testCase "Test for getAttack" $
            getAttack bob @?= 50
        
        , testCase "Test for getHealth" $
            getHealth bob @?= 100

        , testCase "Test for setName" $
            (getName $ setName "Smith" bob) @?= "Smith"
        
        , testCase "Test for setAttack" $
            (getAttack $ setAttack 1 bob) @?= 1

        , testCase "Test for setHealth" $
            (getHealth $ setHealth 1 bob) @?= 1

        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 50, health: 50"

        , testCase "Test for damage" $
            (getHealth $ damage bob 10) @?= 90

        , testCase "Test for isAlive True" $
            isAlive bob @?= True

        , testCase "Test for isAlive False" $
            isAlive scrap @?= False

        , testCase "Alive attacker fights" $
            (getHealth $ fight bob scrap) @?= (-50)

        , testCase "Dead attacker fights" $
            (getHealth $ fight scrap bob) @?= 100

        , testCase "Weak, but alive attacker beats a dead one in a three-round fight unharmed" $
            threeRoundFight weak scrap @?= weak

        , testCase "Weak attacker gets beaten by defender" $
            threeRoundFight weak bob @?= robot "Bob" 50 99

        , testCase "Attacker beats defender in the first round, remaining unharmed" $
            threeRoundFight bob weak @?= bob

        , testCase "Attacker beats defender in the third round" $
            threeRoundFight bob dummy @?= robot "Bob" 50 99
        
        , testCase "Winner takes damage twice" $
            threeRoundFight dummy bob @?= robot "Bob" 50 98
        
        , testCase "Test for neueRobotAttak" $
            (getHealth $ neueRobotAttak dummy) @?= 114

        , testCase "Test survivors" $
            survivors @?= [robot "Pascal" 4 9, robot "Beep" 7 3]
        ]
