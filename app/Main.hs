#! /usr/bin/env -S"ANSWER=42" nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.shower])"
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Map as Map

type Address = String
type Accounts = Map Address Int

data ERC20 = ERC20 {
  totalSupply :: Int
  , name :: String
  , decimals :: Int
  , symbol :: String
  , address :: String
  , accounts :: Accounts
}

class IERC20 a where
  balanceOf :: a -> Address -> Maybe Int
  transfer :: a -> Address -> Address -> Int -> Maybe Accounts
  -- approve :: a -> Address -> Int -> Maybe Int
  -- allowance :: a ->  Address -> Address -> Maybe Int

instance IERC20 ERC20 where
  balanceOf erc20 usrAddr = Map.lookup usrAddr (accounts erc20)
  transfer erc20 alice bob amount =
    if notMember alice acnt || notMember bob acnt then Just acnt else do
        aliceBalance <- Map.lookup alice acnt
        bobBalance <- Map.lookup bob acnt
        if aliceBalance < amount || bobBalance < amount then Just acnt else
          Just $
            update (\balance -> Just (balance - amount)) alice $
            update (\balance -> Just (balance + amount)) bob acnt
    where
      acnt = accounts erc20
  -- approve erc20 alice bob = Just 0
  -- allowance erc20 alice bob = Just 0

unwrap :: Maybe Int -> String
unwrap maybeVal = case maybeVal of
  Nothing -> commonStr <> "0"
  Just i -> commonStr <> (show i)
  where
    commonStr = "You value is: "

main :: IO ()
main = putStrLn joshBalanceStr
  where
    blockchain = Map.fromList [("alice", 1000), ("bob", 3000)]
    erc20 = ERC20 {
        totalSupply = 1000000
        , name = "Ethereuem"
        , decimals = 18
        , symbol = "ETH"
        , address = "0x0001"
        , accounts = blockchain
    }
    updatedAcnt = transfer erc20 "alice" "bob" 2000
    joshBalanceStr =  unwrap $ (updatedAcnt >>= Map.lookup "alice")
