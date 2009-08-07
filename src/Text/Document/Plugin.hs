module Text.Document.Plugin where

import Control.Monad (liftM)
import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec (GenParser)
import qualified Text.XML.Light as XL

import Text.Xml.Xml (Node)
import Misc.Commons (branch, fmap2)

{-
List of formats currently supported for both the parser and the pretty printer
side. Plugins for every format plugins might support parsing or printing.
-}

data Format =
    Wiki
  | XML
  | XHTML
  | LaTeX
  | ADT
  deriving (Eq, Show)

{-
Instead of only allowing text pretty printing for plugins a more flexible
Output type is used. This type should allow plugins to be more precise in their
output domain. E.g. XML/Xhtml pretty printer can simply out XML nodes and do
not have to worry about conversion to strings.
-}

data Output = 
    OutputPlain    String
  | OutputXML      Node        -- Do not use internal nodes.
  | OutputXMLLight XL.Content

data Context = ContextWiki Int
  deriving (Eq, Show)

{-
A plugin contains an identifying name and a transformation function. This
transformation function is indexed by a format identifier and might deliver a
parser for that format. The parse result is a pretty printer function indexed
by a format identifier as well, Given this format the function might deliver a
pretty printed string in that format. Due to these two `Maybe' types the plugin
may support any of the possible combinations of parsing and pretty printing for
arbitrary types.

Because the pretty printer will eventually end up in the document tree, it has
the plugin name included. This way the plugins can easily be identified,
compared or used for error reporting.

Non-pure function can make use of the processor computation that receives a
filepath they can use as a working directory. A bit clumsy, but that's the
burden of purity.
-}

type Parser doc = Format -> Maybe (Maybe Context -> GenParser Char () doc)

data PrettyPrinter doc =
  Printer {
    printerName :: String
  , printer     :: Format -> Maybe (doc -> Output)
  }

data Processor doc sem =
  Processor {
    processorName :: String
  , processor     :: Format -> Maybe (FilePath -> doc -> sem -> IO ())
  }

type Transformer doc sem = Parser (PrettyPrinter doc, Processor doc sem)

data Plugin doc sem =
  Plugin {
    pluginName  :: String
  , transformer :: Transformer doc sem
  }

{-
Plugins are only showable and comparable using there (hopefully) uniquely
identifying name.
-}

instance Eq (PrettyPrinter doc) where
  a == b = printerName a == printerName b

instance Eq (Processor doc sem) where
  a == b = processorName a == processorName b

{-
When a plugin support a pretty printer for the ADT format that produces a plain
string as output, this printer will be used as the show instance for this
plugin.
-}

instance Show (Processor doc sem) where
  show = processorName

instance Show (PrettyPrinter doc) where
  show = printerName

instance Eq (Plugin doc sem) where
  a == b = pluginName a == pluginName b

instance Show (Plugin doc sem) where
  show = pluginName

-------- plugin helper functions ----------------------------------------------

{-
Create a plugin transformer function from both a mapping from format
identifiers to general parsers and a mapping from format identifiers to pretty
printers. The pretty printers should take the result of the parsers.
-}

-- FIX:

makePlugin :: 
     String
  -> [(Format, Maybe Context -> GenParser Char () int)]
  -> [(Format, int -> doc -> Output)]
  -> [(Format, int -> FilePath -> doc -> sem -> IO ())]
  -> Plugin doc sem

makePlugin name ps pps ios = Plugin name $ \fmt -> do
  fmtParser <- lookup fmt ps
  let printer   int fmt' = lookup fmt' pps >>= return . ($int)
  let processor int fmt' = lookup fmt' ios >>= return . ($int)
  return (\a -> 
        fmap2 (Printer name) (Processor name)
    <$> branch printer processor
    <$> fmtParser a)

