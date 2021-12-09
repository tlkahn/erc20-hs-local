#! /usr/bin/env -S"ANSWER=42" nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.shower])"
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Map as Map

type Address = String
type Accounts = Map Address Int
type Allowed = Map Address Accounts

data ERC20 = ERC20 {
  totalSupply :: Int
  , name :: String
  , decimals :: Int
  , symbol :: String
  , address :: String
  , accounts :: Accounts
  , allowance :: Allowed
}

class IERC20 a where
  balanceOf :: a -> Address -> Maybe Int
  transfer :: a -> Address -> Address -> Int -> Maybe Accounts
  approve :: a -> Address -> Address -> Int -> Maybe Allowed

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

  approve erc20 approverAlice approveeBob amount =
    if notMember approverAlice acnt || notMember approveeBob acnt then Just allc  else
      Map.insert approverAlice <$> updatedAliceAllowedActs' <*> Just allc
      where
        allc = allowance erc20
        acnt = accounts erc20
        aliceAllowedAcnts' = Map.lookup approverAlice allc
        updatedAliceAllowedActs' =  Map.insert approveeBob amount <$> aliceAllowedAcnts'

unwrap :: String -> Maybe Int -> String
unwrap commonStr maybeVal = case maybeVal of
  Nothing -> commonStr <> "0"
  Just i -> commonStr <> show i

main :: IO ()
main = putStrLn resultStr
  where
    _blockchain = Map.fromList [("alice", 1000), ("bob", 3000), ("chris", 4000)]
    _allowance = Map.fromList [("alice", Map.fromList [("chris", 100)])]
    erc20 = ERC20 {
        totalSupply = 1000000
        , name = "Ethereuem"
        , decimals = 18
        , symbol = "ETH"
        , address = "0x0001"
        , accounts = _blockchain
        , allowance = _allowance
    }
    updatedAcnt = transfer erc20 "alice" "bob" 200
    updatedAllc = approve erc20 "alice" "chris" 200
    alloc = updatedAllc >>= Map.lookup "alice"
    bobAllc = alloc >>= Map.lookup "chris"
    resultStr = show updatedAcnt <>
                "\n" <>
                show updatedAllc <>
                "\n" <>
                unwrap "chris's allownce is: " bobAllc
