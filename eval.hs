data Expr= ThermType Therm
 |NegThermType Therm
 |Sum Expr Therm
 |Sub Expr Therm 
 deriving Show

data Therm =FaktorType Faktor
 |ProduktType Therm Faktor 
 deriving Show

data Faktor=Number Int
 |ExpressionType Expr 
 deriving Show

evalExpr::Expr->Int
evalExpr (ThermType x)=evalTherm x
evalExpr (NegThermType x)= -(evalTherm x)
evalExpr (Sum y x)=(evalExpr y) + (evalTherm x)
evalExpr (Sub y x)=(evalExpr y) - (evalTherm x)

evalTherm::Therm->Int
evalTherm (FaktorType t)=evalFaktor t
evalTherm (ProduktType g t)=(evalTherm g) * (evalFaktor t)

evalFaktor::Faktor->Int
evalFaktor (Number a)=a
evalFaktor (ExpressionType b)= evalExpr b