module TestExpr where

import Expr
import ExprT
import Parser

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer

testBool = testExp :: Maybe Bool

testMM = testExp :: Maybe MinMax

testMod7 = testExp :: Maybe Mod7

testExprT = testExp :: Maybe ExprT
