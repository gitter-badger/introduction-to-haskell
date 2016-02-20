-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (ns, l) = toRevDigits ns == l

testDoubleEachOther :: ([Integer], [Integer]) -> Bool
testDoubleEachOther (xs, ys) = doubleEveryOther xs == ys

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (xs, y) = sumDigits xs == y

testLuhn :: (Integer, Bool) -> Bool
testLuhn (x, y) = luhn x == y

testHanoi :: ((Integer, Peg, Peg, Peg), [Move]) -> Bool
testHanoi ((n, a, b, c), ms) = hanoi n a b c == ms

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests =  [ Test "toRevDigits test" testToRevDigits
              [(1234, [4,3,2,1]), (12345678, [8,7,6,5,4,3,2,1]), (0, []), (-10, [])]
            ]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEachOther" testDoubleEachOther
    [([4,9,5,5], [4,18,5,10]), ([0, 0], [0, 0]), ([], []), ([1], [1])]
  ]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = [ Test "sumDigits" testSumDigits
    [([10,5,18,4], 19), ([10,11,14,15], 14)]
  ]

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests = [ Test "luhn" testLuhn [(5594589764218858, True), (1234567898765432, False)]]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = [ Test "hanoi" testHanoi [((2, "a", "b", "c"), [("a","c"), ("a","b"), ("c","b")])]]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
