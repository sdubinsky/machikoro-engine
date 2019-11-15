import Lib
import Test.HUnit

testCard = Card {cardName = "Test", cardDescription = "TestCard"}
emptyField = [] :: Field

testAddToField = TestCase $ assertEqual "adds new card to empty list" ([FieldCard {fieldCard = testCard, fieldCardCount = 1 }] :: Field) (addCardToField testCard emptyField)

testAddToFieldTwice = TestCase $ assertEqual "adds new card to empty list" ([FieldCard {fieldCard = testCard, fieldCardCount = 2 }] :: Field) (addCardToField testCard $ addCardToField testCard emptyField)

tests = TestList[TestLabel "field test 1" testAddToField,
                 TestLabel "field test 2" testAddToFieldTwice]

main = runTestTT tests
