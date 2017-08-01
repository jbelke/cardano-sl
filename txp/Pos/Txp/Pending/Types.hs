-- | Types related to pending transactions.

module Pos.Txp.Pending.Types
    ( PendingTx (..)
    , PtxCondition (..)
    ) where

import           Universum

import           Pos.Txp.Core.Types (TxAux, TxId)

data PendingTx = PendingTx
    { ptxTxAux :: TxAux
    , ptxTxId  :: TxId
    } deriving (Eq)

-- | Persistance assessment for given pending transaction.
data PtxCondition
    = PtxApplying        -- ^ Is waiting to be applyed
    | PtxInUpperBlocks   -- ^ Recently appeared in blocks
    | PtxPersisted       -- ^ Transaction is ~guaranteed to remain in blockchain
                         --   (with up to *high* assurance level)
    | PtxWon'tApply Text -- ^ Can't be applyed and requires user's input to
                         --   reform tx
    deriving (Eq, Show)
