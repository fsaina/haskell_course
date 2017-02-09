module Main where

import qualified ExpressionEvaluator as EE

equation = "2 * ( x ^ 2 ) + sin (x+y)"

tree = EE.createExpression equation

sample :: IO Double
sample = return $ EE.evaluate $ EE.substitute 'y' (pi/2) $ EE.substitute 'x' 0 $ tree

derivative :: Char -> IO String
derivative = return .show . EE.deriveBy tree

-- || Sample run of the expression evaluation module
main :: IO ()
main = do
        dx <- derivative 'x'
        dy <- derivative 'y'
        solution <- sample
        putStrLn $ "\nEquation:  " ++ equation
        putStrLn $ "\tDerivative x is: " ++ dx
        putStrLn $ "\tDerivative y is: " ++ dy
        putStrLn $ "\tSolution with x = 0 and y = PI/2 is: \t" ++ (show solution)
