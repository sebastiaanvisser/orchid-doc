module Text.Document.Core.Processing where

import Text.XML.Light (Content)

import Text.Document.Core.Type
import Text.Document.Plugin
import Text.Document.PluginRegister
import Text.Xml.Xml

-- Todo: it feels logical to put the result of side-effects back into the
-- printers. But we do not want to be too ambitious.

processDocument :: Format -> (Syn_Document -> a) -> FilePath -> Document -> IO a
processDocument fmt printer work doc = do

  -- Compute the desired semantics from the document.
  let sem   = wrappedSemDocument doc
      io f  = f work doc sem
      nop   = return ()
      procs = processors_Syn_Document sem

  -- Perform the side-effects that plug-ins may require.
  mapM_ (maybe nop io . flip processor fmt) procs

  -- Contruct the pure result from the document semantics.
  return (printer sem)

-- Wrong place?
processToXHTML :: FilePath -> Document -> IO Node
processToXHTML = processDocument XHTML xhtml_Syn_Document

processToLaTeX :: FilePath -> Document -> IO ShowS
processToLaTeX = processDocument LaTeX latex_Syn_Document

processToXML :: FilePath -> Document -> IO Content
processToXML = processDocument XML xml_Syn_Document

