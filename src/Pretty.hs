{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverloadedStrings #-}

module Pretty
    ( Pretty(..)
    , (<++>), (<+>), (<>)
    , prettyList, prettyListDoc
    , parensIf
    , render, renderNormal, renderWide
    , hang, nest, hcat, hsep, vcat, sep
    , prEmpty, prParens, prBrackets, prBraces
    , Doc
    ) where

-- base
import Text.Printf ( printf )

-- text
import qualified Data.Text as T ( Text, unpack )

-- time
import Data.Time ( UTCTime )

-- pretty
import Text.PrettyPrint
    ( Doc
    , (<>), (<+>)
    , hang, nest, punctuate
    , hcat, vcat, fsep, hsep, sep
    , parens, brackets, braces, empty
    , text
    , style, renderStyle, lineLength
    )


class Show a => Pretty a where
    pretty :: a -> Doc
    prettyPrec :: Int -> a -> Doc

    pretty = prettyPrec 0
    prettyPrec _ = pretty

instance Pretty Doc     where pretty = id
instance Pretty T.Text  where pretty = pretty . T.unpack
instance Pretty String  where pretty = text
instance Pretty ()      where pretty = pretty . show
instance Pretty Bool    where pretty = pretty . show
instance Pretty Int     where pretty = pretty . show
instance Pretty Integer where pretty = pretty . show
instance Pretty Double  where pretty x = pretty (printf "%.2f" x :: String)
instance Pretty UTCTime where pretty = pretty . show

instance (Pretty a, Pretty b) => Pretty (a, b) where
    pretty (a, b) = prettyListDoc parens "," [pretty a, pretty b]

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
    pretty (a, b, c) = prettyListDoc parens "," [pretty a, pretty b, pretty c]

instance Pretty a => Pretty (Maybe a) where
    pretty Nothing  = "Nothing"
    pretty (Just x) = "Just" <+> parens (pretty x)


infixl 5 <++>
(<++>) :: Doc -> Doc -> Doc
a <++> b = hang a 4 b

prettyList :: Pretty a => (Doc -> Doc) -> Doc -> [a] -> Doc
prettyList wrap punc = prettyListDoc wrap punc . map pretty

prettyListDoc :: (Doc -> Doc) -> Doc -> [Doc] -> Doc
prettyListDoc wrap punc = wrap . fsep . punctuate punc

parensIf :: Bool -> Doc -> Doc
parensIf = wrapIf parens
    where
        wrapIf :: (Doc -> Doc) -> Bool -> Doc -> Doc
        wrapIf wrap c = if c then wrap else id

renderNormal :: Pretty a => a -> String
renderNormal = render 120

renderWide :: Pretty a => a -> String
renderWide = render 240

render :: Pretty a => Int -> a -> String
render w = renderStyle (style { lineLength = w }) . pretty

prEmpty :: Doc
prEmpty = empty

prParens :: Doc -> Doc
prParens = parens

prBrackets :: Doc -> Doc
prBrackets = brackets

prBraces :: Doc -> Doc
prBraces = braces
