module Text.Document.Plugin.Formula (formulaPlugin) where

import Control.Applicative
import Control.Exception.Extensible
import Control.Monad (when)
import Data.ByteString (pack)
import Data.Char (ord)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.List (intercalate)
import Misc.Commons (trim)
import Network.Protocol.Uri ((/+))
import System.IO
import System.IO.Unsafe
import System.Posix.Files (getFileStatus, isRegularFile)
import System.Process (runInteractiveProcess, waitForProcess, runProcess)
import Text.Document.Core.Type
import Text.Document.Parser.WikiHelper
import Text.Document.Plugin
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>), try)
import qualified Text.Xhtml.Xhtml as H
import qualified Text.Xml.Xml     as X

-------- hscolour plugin ------------------------------------------------------

formulaPlugin :: Plugin Document Syn_Document
formulaPlugin = makePlugin "formula"
  [(Wiki,  pFormula)]
  [(XHTML, ppXHTML), (LaTeX, ppCodeLaTeX), (ADT, ppADT)]
  [(XHTML, ioXHTML)]

-------- parsers --------------------------------------------------------------

pFormula (Just (ContextWiki n)) = do
   (m, i) <- pIndent n
   (string "@@" <* many (oneOf ws) <* string "formula")
     *> many pEmptyLineEol
     *> (trimCode
    <$> pLevel (m + 1))

-------- pretty printers ------------------------------------------------------

ppXHTML formula doc = OutputXML
  $ X.attribute "class" "formula"
  $ H.img ("_images" /+ generateFileName formula) "formula"

generateFileName :: String -> String
generateFileName formula = md5sum (pack $ map (fromIntegral . ord) formula) ++ ".png"

-------- processors -----------------------------------------------------------

ioXHTML :: String -> FilePath -> Document -> Syn_Document -> IO ()
ioXHTML formula workDir doc sem = do

  let image = generateFileName formula

  exist <- try (isRegularFile <$> getFileStatus (workDir /+ image)) :: IO (Either IOException Bool)
  case exist of
    Right True -> return ()
    Left _  -> do

      -- Save formula pulled through template as tex file.
      writeFile ("/tmp/tex-formula-" ++ image++ ".tex") (texTemplate formula)

      -- Create DVI file from tex.
      runProcess "latex" [
          "-interaction=nonstopmode"
        , "/tmp/tex-formula-" ++ image ++ ".tex"
        ] (Just "/tmp") Nothing Nothing Nothing Nothing
        >>= waitForProcess

      -- Convert DVI file to PNG image.
      runProcess "dvipng" [
          "-q", "-T", "tight", "-bg", "Transparent"
        , "-D", "180", "/tmp/tex-formula-" ++ image ++ ".dvi"
        , "-o", workDir /+ "_images" /+ image
        ] Nothing Nothing Nothing Nothing Nothing
        >>= waitForProcess
      return ()

ppCodeLaTeX s doc = OutputPlain s

ppADT s doc = OutputPlain $ show s

texTemplate :: String -> String
texTemplate s = concat [header, s, footer]
  where
    header = intercalate "\n" [
        "\\documentclass[fleqn]{article}"
      , "\\usepackage{amssymb,amsmath,bm,color}"
      , "\\usepackage[utf8]{inputenc}"
      , "\\begin{document}"
      , "\\thispagestyle{empty}"
      , "\\mathindent0cm"
      , "\\parindent0cm"
      , ""
      ]
    footer = "\n\\end{document}"

