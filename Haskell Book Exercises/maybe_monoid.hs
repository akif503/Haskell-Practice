import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
   mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
  Nada <> (Only a) = Only a
  (Only a) <> Nada = Only a
  (Only a) <> (Only a') = Only (a <> a')
  Nada <> Nada = Nada


-- Madlib
type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin :: Exclamation -> Adverb -> Noun -> Adjective -> String

madlibbin e adv noun adj = 
    mconcat [e, "! he said ", adv, " as he jumped into his car ", 
    noun, " and drove off with his ", adj, " wife."]
