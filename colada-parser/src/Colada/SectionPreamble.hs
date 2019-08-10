{-
Author(s): Jesse Michael Han (2019)

Section preambles
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Colada.SectionPreamble where

import Prelude -- hiding (Int, Bool, String, drop)
import qualified Prelude
import qualified Control.Applicative.Combinators as PC
import Text.Megaparsec hiding (Token, Label, option)
import Control.Monad (guard)
import Control.Monad.Combinators.Expr
import Control.Monad.Trans.State.Lazy
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Text (Text, pack, unpack)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L hiding (symbol, symbol')

import Colada.Basic.Basic
import Colada.Assumption
import Colada.Type
import Colada.PhraseList

import Control.Lens

data SectionPreamble = SectionPreamble SectionTag (Maybe Label)
  deriving (Show, Eq)

maybeLabelOfSectionPreamble :: SectionPreamble -> (Maybe Label)
maybeLabelOfSectionPreamble (SectionPreamble _ ml) = ml

sectionTypeOfSectionPreamble :: SectionPreamble -> Text
sectionTypeOfSectionPreamble (SectionPreamble (SectionTagDocument txts) _) = (head txts)
sectionTypeOfSectionPreamble (SectionPreamble (SectionTagSubdivision txts) _) = (head txts)

parseSectionPreamble_aux :: Parser (SectionPreamble, Int)
parseSectionPreamble_aux = do
  (sec_tag, level) <- parseSectionTag_aux
  (,) <$> (SectionPreamble sec_tag <$> (option parseLabel <* parsePeriod)) <*> return level
  
parseSectionPreamble :: Parser SectionPreamble -- TODO: test this
parseSectionPreamble =
  with_result parse_section_preamble_main modify_section_id
  where
    parse_section_preamble_main :: Parser SectionPreamble
    parse_section_preamble_main =
      fst <$> (with_result parseSectionPreamble_aux (preUpdateStateVec . snd))
      where
        preUpdateStateVec k = modify $ sectionHandler initialFState emptyFState k

    -- when the side effect is called, the new state has already been initialized on the stack
    modify_section_id :: SectionPreamble -> Parser ()
    modify_section_id sp = do
      updateSectionId Locally $ textOfLabel <$> maybeLabelOfSectionPreamble sp
      updateSectionType Locally $ sectionTypeOfSectionPreamble sp

data SectionTag =
    SectionTagDocument [Text]
  | SectionTagSubdivision [Text]
  deriving (Show, Eq)

parseSectionTag :: Parser SectionTag
parseSectionTag =
  SectionTagDocument <$> parseLitDocument <||>
  SectionTagSubdivision <$> parseLitSubdivision

parseSectionTag_aux :: Parser (SectionTag, Int)
parseSectionTag_aux =
  ((_1 %~ SectionTagDocument) <$> parseLitDocument_aux) <||> ((_1 %~ SectionTagSubdivision) <$> parseSubdivision_aux)

data SectionPostamble = SectionPostamble SectionTag (Maybe Label)
  deriving (Show, Eq)

maybeLabelOfSectionPostamble :: SectionPostamble -> (Maybe Label)
maybeLabelOfSectionPostamble (SectionPostamble _ ml) = ml

parseSectionPostamble_aux :: Parser (SectionPostamble, Int)
parseSectionPostamble_aux = do
  (sec_tag, level) <- parseLit "end" *> parseSectionTag_aux
  (,) <$> (SectionPostamble sec_tag <$> ((option parseLabel) <* parsePeriod)) <*> return level
  
parseSectionPostamble :: Parser SectionPostamble -- TODO: test this and ensure only 1 state is popped when leaving a subdivision
parseSectionPostamble =
  fst <$> (with_result parseSectionPostamble_aux (postUpdateStateVec))
  where
    postUpdateStateVec :: (SectionPostamble, Int) -> Parser ()
    postUpdateStateVec ((SectionPostamble tag ml), k) =
      case tag of
        (SectionTagDocument txts) -> do
            let endTag = (head txts)
            startTag <- (use $ top . sectionType)
            let endLabel = textOfLabel <$> ml
            startLabel <- (use $ top . sectionId)
            if (lower_eq startTag endTag && startLabel == endLabel)
              then modify $ sectionHandler initialFState emptyFState k
              else empty
        (SectionTagSubdivision txts) -> do
            let endTag = (head txts)
            startTag <- (use $ top . sectionType)
            let endLabel = textOfLabel <$> ml
            startLabel <- (use $ top . sectionId)
            if (lower_eq startTag endTag && startLabel == endLabel)
              then modify $ sectionHandler initialFState emptyFState (k - 2)
              else empty
-- note: parseSubdivision_aux always adds 1 to the stack depth
