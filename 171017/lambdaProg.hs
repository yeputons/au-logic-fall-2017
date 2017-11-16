module LambdaProg(church, churchS, churchSum, churchMul) where
import Lambda

church :: Int -> Prog
church 0 = "s" :=> "z" :=> Ref "z"
church n = "s" :=> "z" :=> Ref "s" :@: ((church $ n-1) :@: Ref "s" :@: Ref "z")

churchS = "x" :=> "s" :=> "z" :=> Ref "s" :@: (Ref "x" :@: Ref "s" :@: Ref "z")
churchSum = "a" :=> "b" :=> "s" :=> "z" :=> (Ref "a" :@: Ref "s") :@: ((Ref "b" :@: Ref "s" :@: Ref "z"))
churchMul = "a" :=> "b" :=> "s" :=> (Ref "a" :@: (Ref "b" :@: Ref "s"))

