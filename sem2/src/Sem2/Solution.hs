module Sem2.Solution where

import Sem2.Types
import Data.Either (isLeft)

type Context = [(Symbol, Type)];

contextExtend :: Context -> (Symbol, Type) -> Context
contextExtend ctx pair = pair : ctx

contextLookup :: Context -> Symbol -> Either String Type
contextLookup ctx sym = case lookup sym ctx of
                          Just t -> Right t
                          Nothing -> Left $ "Unknown variable " ++ show sym

typeOf :: Term -> Either String Type
typeOf term = typeOf' [] term

typeOf' :: Context -> Term -> Either String Type


typeOf' ctx (Sym sym) = contextLookup ctx sym

typeOf' ctx (Lam sym type' term) =
    case typeInner of
      Left e -> Left e
      Right typeRes -> Right $ Fun type' typeRes
    where
        typeInner = typeOf' (contextExtend ctx (sym, type')) term

typeOf' ctx (App term1 term2) =
    case type1 of
      Left e -> Left e
      Right (Fun ftype1 ftype2)
            | Right ftype1 == type2 -> Right ftype2
            | otherwise -> Left "Type mismatch on Function Application"
      Right _ -> Left "Left arg of App must be a Fun"
    where
        type1 = typeOf' ctx term1
        type2 = typeOf' ctx term2

--

typeOf' _ (Natural int) | int >= 0 = Right Nat
                        | otherwise = Left "Natural must be >= 0"

typeOf' ctx (Add term1 term2)
        | isLeft type1 = type1
        | isLeft type2 = type2
        | type1 /= Right Nat = Left "Left arg of an App must be a Nat"
        | type2 /= Right Nat = Left "Right arg of an App must be a Nat"
        | otherwise = Right Nat
    where
        type1 = typeOf' ctx term1
        type2 = typeOf' ctx term2

typeOf' ctx (Mult term1 term2)
        | isLeft type1 = type1
        | isLeft type2 = type2
        | type1 /= Right Nat = Left "Left arg of a Mult must be a Nat"
        | type2 /= Right Nat = Left "Right arg of a Mult must be a Nat"
        | otherwise = Right Nat
    where
        type1 = typeOf' ctx term1
        type2 = typeOf' ctx term2

--

typeOf' ctx (Boolean _) = Right Bool

typeOf' ctx (Not term)
        | isLeft type' = type'
        | type' == Right Bool = Right Bool
        | otherwise = Left "Arg of Not is not a Bool"
    where
        type' = typeOf' ctx term

typeOf' ctx (And term1 term2)
        | isLeft type1 = type1
        | isLeft type2 = type2
        | type1 /= Right Bool = Left "Left arg of an And must be a Bool"
        | type2 /= Right Bool = Left "Right arg of an And must be a Bool"
        | otherwise = Right Bool
   where
        type1 = typeOf' ctx term1
        type2 = typeOf' ctx term2

typeOf' ctx (Or term1 term2)
        | isLeft type1 = type1
        | isLeft type2 = type2
        | type1 /= Right Bool = Left "Left arg of an Or must be a Bool"
        | type2 /= Right Bool = Left "Right arg of an Or must be a Bool"
        | otherwise = Right Bool
    where
        type1 = typeOf' ctx term1
        type2 = typeOf' ctx term2

typeOf' ctx (Iff condTerm thenTerm elseTerm)
        | isLeft condType = condType
        | isLeft thenType = thenType
        | isLeft elseType = elseType
        | condType /= Right Bool = Left "Condition of an Iff must be a Bool"
        | thenType /= elseType = Left "Then and Else args of an Iff must not have different types"
        | otherwise = thenType
    where
        condType = typeOf' ctx condTerm
        thenType = typeOf' ctx thenTerm
        elseType = typeOf' ctx elseTerm

--

typeOf' ctx (Pair term1 term2) =
    case type1 of
      Left e1 -> Left e1
      Right rType1 -> case type2 of
                        Left e2 -> Left e2
                        Right rType2 -> Right $ PairT rType1 rType2
    where
        type1 = typeOf' ctx term1
        type2 = typeOf' ctx term2

typeOf' ctx (Fst term) =
    case type' of
      Left e -> Left e
      Right (PairT firstType _) -> Right firstType
      Right _ -> Left "Arg of Fst must be of PairT type"
    where
        type' = typeOf' ctx term

typeOf' ctx (Snd term) =
    case type' of
      Left e -> Left e
      Right (PairT _ secondType) -> Right secondType
      Right _ -> Left "Arg of Snd must be of PairT type"
    where
        type' = typeOf' ctx term

--

typeOf' ctx (Cons term1 term2) =
    case type2 of
      Left e -> Left e
      Right (List listType) ->
          case type1 of
            Left e -> Left e
            Right rType1 | rType1 == listType -> Right $ List listType
                         | rType1 /= listType -> Left "First arg of Cons must have the same type as the List elements"
      Right _ -> Left "Second arg of Cons must be of List type"
    where
        type1 = typeOf' ctx term1
        type2 = typeOf' ctx term2

typeOf' ctx (Nil type') = Right $ List type'

typeOf' ctx (IsNil term) =
    case type' of
      Left e -> Left e
      Right (List _) -> Right Bool
      Right _ -> Left "Arg of IsNil must be of List type"
    where
        type' = typeOf' ctx term

typeOf' ctx (Head term) =
    case type' of
      Left e -> Left e
      Right (List listType) -> Right listType
      Right _ -> Left "Arg of Head must be of List type"
    where
        type' = typeOf' ctx term

typeOf' ctx (Tail term) =
    case type' of
      Left e -> Left e
      Right (List listType) -> Right $ List listType
      Right _ -> Left "Arg of Tail must be of List type"
    where
        type' = typeOf' ctx term

