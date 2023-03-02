module Main (main) where

import Data.String.Interpolate (__i)
import Data.Text.Lazy (Text)
import Eval
import Parser
import Test.Hspec
import Test.QuickCheck

lash :: (a -> c) -> Either a b -> Either c b
lash f (Left a) = Left (f a)
lash f (Right b) = Right b

parseEval :: Text -> Either String Object
parseEval exprStr =
  (lash show . fst . runEval)
    =<< lash errorBundlePretty (runParser pFile "unit test" exprStr)

main :: IO ()
main = do
  hspec do
    describe "Eval maths" do
      it "let ... in ... expression" $
        property \x y ->
          Right (PrimInt (x + y))
            == parseEval
              [__i|
                let x = #{x};
                    y = #{y};
                in x + y
              |]
      it "function call" $
        property \x y ->
          Right (PrimInt (x + y))
            == parseEval
              [__i|
                (|x y| x + y) (#{x}) (#{y})
              |]
      it "closure" $
        property \x ->
          Right (PrimInt (x + 5))
            == parseEval
              [__i|
                let mkAdder = |x| |y| x + y;
                    compose = |f g| |x| f (g x);
                    add2 = mkAdder 2;
                    add3 = mkAdder 3;
                in (compose add2 add3) (#{x})
              |]
