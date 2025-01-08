import Test.HUnit
import GameOfLife
import Const

test1 :: Test
test1 = TestCase (assertEqual "Test 1: cellInBounds 1 2" True (cellInBounds 1 2))

test2 :: Test
test2 = TestCase (assertEqual "Test 2: cellInBounds 0 0" True (cellInBounds 0 0))

test3 :: Test
test3 = TestCase (assertEqual "Test 3: cellInBounds -1 2" False (cellInBounds (-1) 2))

--For row/column=100
test4 :: Test
test4 = TestCase (assertEqual "Test 4: allCells" (rows * columns) (length allCells))

test5 :: Test
test5 = TestCase (assertEqual "Test 5: gameStep" [(1, 1), (1, 2), (2, 1), (2, 2)] (gameStep [(1, 1), (1, 2), (2, 1), (2, 2)]))

test6 :: Test
test6 = TestCase (assertEqual "Test 6: gameStep" [(1, 2), (2, 2), (2, 3)] (gameStep [(2, 1), (2, 2), (2, 3)]))

test7 :: Test
test7 = TestCase (assertEqual "Test 7: allCells" [] (gameStep []))

-- Setup
tests :: Test
tests = TestList [test1, test2, test3, test4, test5, test6, test7]

main :: IO ()
main = runTestTT tests >>= print