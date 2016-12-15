-- | Type class abstracting UTXO (set of unspent outputs).

module Pos.Types.Utxo.Class
       ( MonadUtxoRead (..)
       , MonadUtxo (..)
       ) where

import           Universum

import           Pos.Types.Types (TxIn, TxOut)

class Monad m => MonadUtxoRead m where
    utxoGet :: TxIn -> m (Maybe TxOut)

class MonadUtxoRead m => MonadUtxo m where
    utxoPut :: TxIn -> TxOut -> m ()
    utxoDel :: TxIn -> m ()
