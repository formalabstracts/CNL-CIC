{-
Author(s): Jesse Michael Han (2019)

Parser state.
-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module CNLean.Basic.State where

import Prelude
import Control.Monad.Trans.State
import qualified Prelude
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack, unpack, toLower)
import Data.Void
import Control.Monad (guard)
import qualified Data.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

data Patt = Wd [Text] | Sm Text | Vr | Nm deriving (Eq, Show)

data FState = FState {
  adjExpr, verExpr, ntnExpr, sntExpr :: [[Patt]],
  cfnExpr, rfnExpr, lfnExpr, ifnExpr :: [[Patt]],
  cprExpr, rprExpr, lprExpr, iprExpr :: [[Patt]],

  -- tvrExpr :: [TVar] -- TODO(jesse) integrate this later
  strSyms :: [[Text]], varDecl :: [Text],
  idCount :: Int, hiddenCount :: Int, serialCounter :: Int}

updateSerialCounter fs x = fs {serialCounter = x}
updateHiddenCount fs x = fs {hiddenCount = x}
updateIdCount fs x = fs {idCount = x}
updateVarDecl fs x = fs {varDecl = x}
updateStrSyms fs x = fs {strSyms = x}

updateIprExpr fs x = fs {iprExpr = x}
updateConsIprExpr fs z = fs {iprExpr = (z:(iprExpr fs))}

updateLprExpr fs x = fs {lprExpr = x}
updateConsLprExpr fs z = fs {lprExpr = (z:(lprExpr fs))}

updateRprExpr fs x = fs {rprExpr = x}
updateConsRprExpr fs z = fs {rprExpr = (z:(rprExpr fs))}

updateCprExpr fs x = fs {cprExpr = x}
updateConsCprExpr fs z = fs {cprExpr = (z:(cprExpr fs))}

updateIfnExpr fs x = fs {ifnExpr = x}
updateConsIfnExpr fs z = fs {ifnExpr = (z:(ifnExpr fs))}

updateLfnExpr fs x = fs {lfnExpr = x}
updateConsLfnExpr fs z = fs {lfnExpr = (z:(lfnExpr fs))}

updateRfnExpr fs x = fs {rfnExpr = x}
updateConsRfnExpr fs z = fs {rfnExpr = (z:(rfnExpr fs))}

updateCfnExpr fs x = fs {cfnExpr = x}
updateConsCfnExpr fs z = fs {cfnExpr = (z:(cfnExpr fs))}

updateSntExpr fs x = fs {sntExpr = x}
updateConsSntExpr fs z = fs {sntExpr = (z:(sntExpr fs))}

updateNtnExpr fs x = fs {ntnExpr = x}
updateConsNtnExpr fs z = fs {ntnExpr = (z:(ntnExpr fs))}

updateVerExpr fs x = fs {verExpr = x}
updateConsVerExpr fs z = fs {verExpr = (z:(verExpr fs))}

updateAdjExpr fs x = fs {adjExpr = x}
updateConsAdjExpr fs z = fs {adjExpr = (z:(adjExpr fs))}


initialFState :: FState
initialFState = FState
  adjE0 []    ntnE0 sntE0
  cfnE0 rfnE0 []    []
  []    []    []    iprE0
  []    []
  0 0 0
  where
  adjE0 = [[Wd ["equal"], Wd ["to"], Vr],
           [Wd ["nonequal"], Wd ["to"], Vr]]
          
  ntnE0 = [([Wd ["function","functions"], Nm]),
           ([Wd ["set","sets"], Nm]),
           ([Wd ["element", "elements"], Nm, Wd ["of"], Vr]),
           ([Wd ["object", "objects"], Nm])]
          
  sntE0 = [([Sm "=", Vr])]
  
  cfnE0 = [([Sm "Dom", Sm "(",Vr,Sm ")"]),
           ([Sm "(", Vr, Sm ",", Vr, Sm ")"]) ]
          
  rfnE0 = [[Sm "[", Vr, Sm "]"]]
  
  iprE0 = [([Sm "="]),
           ([Sm "!", Sm "="]),
           ([Sm "-", Sm "<", Sm "-"]),
           ([Sm "-~-"]) ]
