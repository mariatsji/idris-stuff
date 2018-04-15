module Main

import Data.Vect

data DataStore : Type where
     MkDataStore : (size : Nat) ->
                   (items : Vect size String) ->
                   DataStore

data Command = Add String | Get Integer | Quit

size : DataStore -> Nat
size (MkDataStore size' items) = size'

items : (store : DataStore) -> Vect (size store) String
items (MkDataStore size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkDataStore size' items') newItem = MkDataStore _ (addToData items')
  where addToData : Vect n String -> Vect (S n) String
        addToData [] = [newItem]
        addToData (x :: xs) = x :: addToData xs

emptyStore : DataStore
emptyStore = MkDataStore 0 []

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" args = Just $ Add args
parseCommand "get" args = case all isDigit (unpack args) of
                          False => Nothing
                          True => Just $ Get (cast args)
parseCommand "quit" _ = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/=' ') input of
                   (cmd, args) => parseCommand cmd args

getEntry : (k : Integer) -> (ds : DataStore) -> (input : String) -> Maybe (String, DataStore)
getEntry pos store input = let store_items = items store in
                               case integerToFin pos (size store) of
                                    Nothing => Just ("Out of range\n", store)
                                    Just id => Just (index id store_items ++ "\n", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput ds input = case parse input of
                             Just (Add x) => Just ("ID " ++ show (size ds) ++ "\n", addToStore ds x)
                             Just (Get k) => getEntry k ds input
                             Just Quit => Nothing
                             Nothing => Just ("Invalid command\n", ds)



main : IO ()
main = replWith emptyStore "Command: " processInput
