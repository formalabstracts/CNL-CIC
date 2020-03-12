{-
Author(s): Jesse Michael Han (2019)

Section preambles
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Colada.SectionPreamble where

import Prelude hiding (Word) -- hiding (Int, Bool, String, drop)
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

sectionTagOfSectionPreamble :: SectionPreamble -> Text
sectionTagOfSectionPreamble (SectionPreamble (SectionTagDocument txts) _) = (head txts)
sectionTagOfSectionPreamble (SectionPreamble (SectionTagSubdivision txts) _) = (head txts)

parseSectionPreamble_aux :: Parser (SectionPreamble, Int)
parseSectionPreamble_aux = do
  (sec_tag, level) <- parseSectionTag_aux
  (,) <$> (SectionPreamble sec_tag <$> (option parseLabel <* parsePeriod)) <*> return level
  
parseSectionPreamble :: Parser SectionPreamble -- TODO: test this
parseSectionPreamble =
  (with_result parse_section_preamble_main (modify_section_id_and_tag)) <* sc
  where
    parse_section_preamble_main :: Parser SectionPreamble
    parse_section_preamble_main =
      fst <$> (with_result parseSectionPreamble_aux (preUpdateStateVec . snd))
      where
        preUpdateStateVec k = modify $ sectionPreambleHandler initialFState emptyFState k

    -- when the side effect is called, the new state has already been initialized on the stack
    modify_section_id_and_tag :: SectionPreamble -> Parser ()
    modify_section_id_and_tag sp = do
      updateSectionId Locally $ textOfLabel <$> maybeLabelOfSectionPreamble sp
      updateSectionTag Locally $ (pure $ sectionTagOfSectionPreamble (sp))

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
        (SectionTagDocument txts) -> do -- txts is of the form ["section"], ["subsection"], etc
            let endTag = (head txts) -- extract the literal section tag
            startTag <- (use $ top . sectionTag) -- extract the section tag of the local scope
            let endLabel = textOfLabel <$> ml -- get literal section label
            startLabel <- (use $ top . sectionId) -- extract the section label of the local scope
            if (maybe_lower_eq startTag (pure endTag) && startLabel == endLabel)
              then modify $ popNothingSectionTags . (popStateVec 1) -- the statevec pops 1 FState, then pops all dummy FStates which were inserted if going from e.g. a section to a subsubsection
              else fail "section postamble parse failed: section tags or section labels are unequal"
        (SectionTagSubdivision txts) -> do
            let endTag = (head txts)
            startTag <- (use $ top . sectionTag)
            let endLabel = textOfLabel <$> ml
            startLabel <- (use $ top . sectionId)
            if (maybe_lower_eq startTag (pure endTag) && startLabel == endLabel)
              then modify $ popNothingSectionTags . (popStateVec 1)
              else empty
-- note: parseSubdivision_aux always adds 1 to the stack depth
