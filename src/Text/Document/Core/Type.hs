

-- UUAGC 0.9.7 (src/Text/Document/Core/Type.ag)
module Text.Document.Core.Type where

import Prelude hiding (span, div)
import Data.List (intercalate)
import qualified Text.XML.Light as XL

import Text.Document.Plugin
import Text.Xhtml.Xhtml
import Text.Xml.Xml
import Misc.Commons (fmap2, intersperseS, concatMapS, unlinesS, concatS, eolS, ss)
import qualified Text.Xhtml.Xhtml as H

data LinkType =
    Internal
  | External
  | Reference
  deriving (Eq, Show)




document   = Document_Document  
section    = Structure_Section   
blocks     = Block_Blocks    
paragraph  = Block_Paragraph 
enum       = Block_Enum
list       = Block_List
mapping    = Block_Mapping
verbatim   = Block_Verbatim
comment    = Block_Comment
include    = Block_Include
anchor_    = Block_Anchor
caption    = Block_Caption
plugin     = Block_Plugin

lines      = Inline_Lines     
image      = Inline_Image     
text       = Inline_Text      
emph       = Inline_Emph      
strong     = Inline_Strong    
fixed      = Inline_Fixed
link       = Inline_Link      
footnote   = Inline_Footnote  
quote      = Inline_Quote     

(Inline_Text a) <++> (Inline_Text b) = Inline_Text (a ++ b)

-- Wrapper functions for semantic domains.
wrappedSemDocument doc = flip (wrap_Document . sem_Document) (Inh_Document doc) doc




toXHTML :: Document -> Node
toXHTML = xhtml_Syn_Document . wrappedSemDocument

xhtmlDocument t s anns ab b =
  classDiv "document" $ nodeset (
      classDiv "header" (nodeset ([
        h1 t
      , withClass "sub" h1 s
--       , classDiv "author" (nodeset [maybe empty id (lookup "author" anns)])
--       , classDiv "enmail" (nodeset [maybe empty id (lookup "email" anns)])
    ] ++ map (\(a, b) -> classDiv ("annotation " ++ a) (nodeset [b])) anns)
    ) : xhtmlAbstract ab : b)

-- Create the HTML block containing the document abstract.
xhtmlAbstract ab
  | ab == empty = empty
  | otherwise =
      classDiv "abstract"
    $ nodeset [h2 $ H.text "Abstract", ab]

xhtmlSection _   _ False _ b = div $ nodeset b
xhtmlSection lvl s True  t b = div $ nodeset (hdr : b)
  where
    title  = nodeset [span $ H.text number, t]
    hdr    = header (lvl+2) title
    number = (intercalate "." $ map show $ reverse s) ++ " "

xhtmlCleanList xs = if null items then [] else [ul $ items]
  where items = filter significant xs

xhtmlLink kind  = withClass (show kind)

linkAnchor Reference ref = anchor ('#':ref)
linkAnchor Internal  ref = anchor (ref ++ ".html")
linkAnchor _         ref = anchor ref

xhtmlFromOutput (OutputPlain s) = H.text s
xhtmlFromOutput (OutputXML   n) = n

xhtmlPlugin pp doc = maybe err (\a -> xhtmlFromOutput $ a doc) $ printer pp XHTML
  where
    err = classSpan "format-error"
        $ H.text ("No XHTML pretty printer for plugin: " ++ printerName pp)



toXML :: Document -> XL.Content
toXML = xml_Syn_Document . wrappedSemDocument

-- todo: type signatures

xmlElem name childs =
  XL.Elem $ XL.Element (XL.QName name Nothing Nothing) [] childs Nothing

xmlText text = XL.Text (XL.CData XL.CDataText text Nothing)

xmlFromOutput (OutputXMLLight s) = s

xmlPlugin pp doc = maybe err (\a -> xmlFromOutput $ a doc) $ printer pp XML
  where
    err = xmlText ("No XML pretty printer for plugin: " ++ printerName pp)

-- Very `rough' function to strip the outer element off.
xmlStrip :: XL.Content -> XL.Content
xmlStrip (XL.Elem (XL.Element _ _ (c:_) _)) = c




toLaTeX :: Document -> String
toLaTeX = ($"") . latex_Syn_Document . wrappedSemDocument

-- Escape special characters in text that will end up in an LaTeX document.
escapeTex :: String -> String
escapeTex []       = []
escapeTex ('\\':xs) = "\\\\"   ++ escapeTex xs
escapeTex (x:xs)   = x : escapeTex xs

-- Shortcut functions for common tex constructs.

ifEmpty   s t e = if not (null (s "")) then t else e
ifEmptyId s t = ifEmpty s t id

texBlock    n c   = ss "\\begin{" . ss n . ss "}\n" . c . ss "\\end{" . ss n . ss "}\n"
texLine     n c   = ss "\\" . ss n . ss "{\n" . c . ss "}\n"
texSingle   n     = ss "\\" . ss n . ss "\n"
texStart    n     = ss "\\" . ss n . ss "\n"
texStartP   n b p = ss "\\" . ss n . ifEmptyId b (ss "[" . b . ss "]") . ss "{" . p . ss "}\n"
texSurround n p   = ss "\\" . ss n . ss "{" . p . ss "}"

-- Shortcut functions specifc latex constructs.

texDocument t s a b =
    texArticle
  . texTitle (t . ss "\\\\" . s)
  . texBlock "document"
      (texMaketitle . texAbstract a . b)

texArticle        = texStartP   "documentclass" id (ss "article")
                  . texStartP   "usepackage" id (ss "amssymb,amsmath,bm,color")
                  . texStartP   "usepackage" (ss "utf8") (ss "inputenc")
                  . texStartP   "usepackage" id (ss "hyperref")
texTitle          = texStartP   "title" id
texMaketitle      = texSingle   "maketitle"
texAbstract       = texBlock    "abstract"
texVerbatim       = texBlock    "verbatim"
texSection n      = texStartP   (concat (replicate n "sub") ++ "section") id
texItemize        = texBlock    "itemize"
texEnumerate      = texBlock    "enumerate"
texItem           = texStart    "item"
texEmph           = texSurround "emph"
texStrong         = texSurround "textbf"
texFixed          = texSurround "texttt"
texLink a b       = texSurround "url" (ss a) 
texFootnote       = texSurround "footnote"
texMapping (x, y) = texSurround "item" x . eolS . y . eolS

-- Plugin handler.

texPluginErr n = ss ("ERROR: unable to pretty print plugin: " ++ n) . eolS . eolS

texFromOutput _ (OutputPlain s) = ss s
texFromOutput n _               = (texPluginErr n)

texPlugin pp doc =
  let n = printerName pp in
  maybe (texPluginErr n) (\a -> texFromOutput n $ a doc) (printer pp LaTeX)

makeTexSection ist title level body =
  (if ist then texSection (level - 1) title else id)
  . intersperseS (eolS . eolS) body



documentToXhtmlTOC :: Document -> NodeSet
documentToXhtmlTOC = xhtmlTOC_Syn_Document . wrappedSemDocument
-- Annotation --------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         xhtml                : (String, Node)
         xml                  : XL.Content
   alternatives:
      alternative Tuple:
         child x1             : {String}
         child x2             : Inline 
-}
type Annotation  = ( (String),(Inline))
-- cata
sem_Annotation :: Annotation  ->
                  T_Annotation 
sem_Annotation ( x1,x2)  =
    (sem_Annotation_Tuple x1 (sem_Inline x2 ) )
-- semantic domain
type T_Annotation  = ( ((String, Node)),(XL.Content))
data Inh_Annotation  = Inh_Annotation {}
data Syn_Annotation  = Syn_Annotation {xhtml_Syn_Annotation :: (String, Node),xml_Syn_Annotation :: XL.Content}
wrap_Annotation :: T_Annotation  ->
                   Inh_Annotation  ->
                   Syn_Annotation 
wrap_Annotation sem (Inh_Annotation )  =
    (let ( _lhsOxhtml,_lhsOxml) =
             (sem )
     in  (Syn_Annotation _lhsOxhtml _lhsOxml ))
sem_Annotation_Tuple :: String ->
                        T_Inline  ->
                        T_Annotation 
sem_Annotation_Tuple x1_ x2_  =
    (let _lhsOxhtml :: ((String, Node))
         _lhsOxml :: (XL.Content)
         _x2Ilatex :: ShowS
         _x2Ixhtml :: Node
         _x2Ixml :: (XL.Content)
         -- "src/Text/Document/Printer/Xhtml.ag"(line 76, column 16)
         _lhsOxhtml =
             (x1_, _x2Ixhtml)
         -- "src/Text/Document/Printer/Xml.ag"(line 57, column 16)
         _lhsOxml =
             xmlElem "annotation" [
               xmlElem "key"   [xmlText x1_]
             , xmlElem "value" [_x2Ixml]
             ]
         ( _x2Ilatex,_x2Ixhtml,_x2Ixml) =
             (x2_ )
     in  ( _lhsOxhtml,_lhsOxml))
-- Annotations -------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         xhtml                : [(String, Node)]
         xml                  : [XL.Content]
   alternatives:
      alternative Cons:
         child hd             : Annotation 
         child tl             : Annotations 
      alternative Nil:
-}
type Annotations  = [(Annotation)]
-- cata
sem_Annotations :: Annotations  ->
                   T_Annotations 
sem_Annotations list  =
    (Prelude.foldr sem_Annotations_Cons sem_Annotations_Nil (Prelude.map sem_Annotation list) )
-- semantic domain
type T_Annotations  = ( ([(String, Node)]),([XL.Content]))
data Inh_Annotations  = Inh_Annotations {}
data Syn_Annotations  = Syn_Annotations {xhtml_Syn_Annotations :: [(String, Node)],xml_Syn_Annotations :: [XL.Content]}
wrap_Annotations :: T_Annotations  ->
                    Inh_Annotations  ->
                    Syn_Annotations 
wrap_Annotations sem (Inh_Annotations )  =
    (let ( _lhsOxhtml,_lhsOxml) =
             (sem )
     in  (Syn_Annotations _lhsOxhtml _lhsOxml ))
sem_Annotations_Cons :: T_Annotation  ->
                        T_Annotations  ->
                        T_Annotations 
sem_Annotations_Cons hd_ tl_  =
    (let _lhsOxhtml :: ([(String, Node)])
         _lhsOxml :: ([XL.Content])
         _hdIxhtml :: ((String, Node))
         _hdIxml :: (XL.Content)
         _tlIxhtml :: ([(String, Node)])
         _tlIxml :: ([XL.Content])
         -- use rule "src/Text/Document/Printer/Xhtml.ag"(line 59, column 39)
         _lhsOxhtml =
             _hdIxhtml : _tlIxhtml
         -- use rule "src/Text/Document/Printer/Xml.ag"(line 45, column 13)
         _lhsOxml =
             _hdIxml : _tlIxml
         ( _hdIxhtml,_hdIxml) =
             (hd_ )
         ( _tlIxhtml,_tlIxml) =
             (tl_ )
     in  ( _lhsOxhtml,_lhsOxml))
sem_Annotations_Nil :: T_Annotations 
sem_Annotations_Nil  =
    (let _lhsOxhtml :: ([(String, Node)])
         _lhsOxml :: ([XL.Content])
         -- use rule "src/Text/Document/Printer/Xhtml.ag"(line 59, column 39)
         _lhsOxhtml =
             []
         -- use rule "src/Text/Document/Printer/Xml.ag"(line 45, column 13)
         _lhsOxml =
             []
     in  ( _lhsOxhtml,_lhsOxml))
-- Block -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         sem                  : Document 
      synthesized attributes:
         latex                : ShowS
         processors           : [Processor Document Syn_Document]
         xhtml                : Node
         xml                  : XL.Content
   alternatives:
      alternative Anchor:
         child name           : {String}
         child contents       : Block 
      alternative Blocks:
         child blocks         : Blocks 
      alternative Caption:
         child caption        : Inline 
         child contents       : Block 
      alternative Comment:
         child comment        : {String}
      alternative Enum:
         child items          : Blocks 
      alternative Include:
         child document       : {String}
      alternative List:
         child items          : Blocks 
      alternative Mapping:
         child pairs          : Pairs 
      alternative Paragraph:
         child paragraph      : Inline 
      alternative Plugin:
         child plugin         : {(PrettyPrinter Document, Processor Document Syn_Document)}
      alternative Verbatim:
         child verbatim       : Block 
-}
data Block  = Block_Anchor (String) (Block) 
            | Block_Blocks (Blocks) 
            | Block_Caption (Inline) (Block) 
            | Block_Comment (String) 
            | Block_Enum (Blocks) 
            | Block_Include (String) 
            | Block_List (Blocks) 
            | Block_Mapping (Pairs) 
            | Block_Paragraph (Inline) 
            | Block_Plugin ((PrettyPrinter Document, Processor Document Syn_Document)) 
            | Block_Verbatim (Block) 
            deriving ( Eq,Show)
-- cata
sem_Block :: Block  ->
             T_Block 
sem_Block (Block_Anchor _name _contents )  =
    (sem_Block_Anchor _name (sem_Block _contents ) )
sem_Block (Block_Blocks _blocks )  =
    (sem_Block_Blocks (sem_Blocks _blocks ) )
sem_Block (Block_Caption _caption _contents )  =
    (sem_Block_Caption (sem_Inline _caption ) (sem_Block _contents ) )
sem_Block (Block_Comment _comment )  =
    (sem_Block_Comment _comment )
sem_Block (Block_Enum _items )  =
    (sem_Block_Enum (sem_Blocks _items ) )
sem_Block (Block_Include _document )  =
    (sem_Block_Include _document )
sem_Block (Block_List _items )  =
    (sem_Block_List (sem_Blocks _items ) )
sem_Block (Block_Mapping _pairs )  =
    (sem_Block_Mapping (sem_Pairs _pairs ) )
sem_Block (Block_Paragraph _paragraph )  =
    (sem_Block_Paragraph (sem_Inline _paragraph ) )
sem_Block (Block_Plugin _plugin )  =
    (sem_Block_Plugin _plugin )
sem_Block (Block_Verbatim _verbatim )  =
    (sem_Block_Verbatim (sem_Block _verbatim ) )
-- semantic domain
type T_Block  = Document ->
                ( ShowS,([Processor Document Syn_Document]),Node,(XL.Content))
data Inh_Block  = Inh_Block {sem_Inh_Block :: Document}
data Syn_Block  = Syn_Block {latex_Syn_Block :: ShowS,processors_Syn_Block :: [Processor Document Syn_Document],xhtml_Syn_Block :: Node,xml_Syn_Block :: XL.Content}
wrap_Block :: T_Block  ->
              Inh_Block  ->
              Syn_Block 
wrap_Block sem (Inh_Block _lhsIsem )  =
    (let ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml) =
             (sem _lhsIsem )
     in  (Syn_Block _lhsOlatex _lhsOprocessors _lhsOxhtml _lhsOxml ))
sem_Block_Anchor :: String ->
                    T_Block  ->
                    T_Block 
sem_Block_Anchor name_ contents_  =
    (\ _lhsIsem ->
         (let _lhsOxhtml :: Node
              _lhsOxml :: (XL.Content)
              _lhsOlatex :: ShowS
              _lhsOprocessors :: ([Processor Document Syn_Document])
              _contentsOsem :: Document
              _contentsIlatex :: ShowS
              _contentsIprocessors :: ([Processor Document Syn_Document])
              _contentsIxhtml :: Node
              _contentsIxml :: (XL.Content)
              -- "src/Text/Document/Printer/Xhtml.ag"(line 90, column 16)
              _lhsOxhtml =
                  nameAnchor name_ _contentsIxhtml
              -- "src/Text/Document/Printer/Xml.ag"(line 88, column 16)
              _lhsOxml =
                  xmlElem "anchor"    [xmlText name_, _contentsIxml]
              -- "src/Text/Document/Printer/Latex.ag"(line 108, column 16)
              _lhsOlatex =
                  ss "-- todo Anchor --"
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  _contentsIprocessors
              -- copy rule (down)
              _contentsOsem =
                  _lhsIsem
              ( _contentsIlatex,_contentsIprocessors,_contentsIxhtml,_contentsIxml) =
                  (contents_ _contentsOsem )
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml)))
sem_Block_Blocks :: T_Blocks  ->
                    T_Block 
sem_Block_Blocks blocks_  =
    (\ _lhsIsem ->
         (let _lhsOxhtml :: Node
              _lhsOxml :: (XL.Content)
              _lhsOlatex :: ShowS
              _lhsOprocessors :: ([Processor Document Syn_Document])
              _blocksOsem :: Document
              _blocksIlatex :: ([ShowS])
              _blocksIprocessors :: ([Processor Document Syn_Document])
              _blocksIxhtml :: NodeSet
              _blocksIxml :: ([XL.Content])
              -- "src/Text/Document/Printer/Xhtml.ag"(line 82, column 16)
              _lhsOxhtml =
                  nodeset _blocksIxhtml
              -- "src/Text/Document/Printer/Xml.ag"(line 80, column 16)
              _lhsOxml =
                  xmlElem "blocks"    _blocksIxml
              -- "src/Text/Document/Printer/Latex.ag"(line 102, column 16)
              _lhsOlatex =
                  intersperseS (eolS . eolS) _blocksIlatex
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  _blocksIprocessors
              -- copy rule (down)
              _blocksOsem =
                  _lhsIsem
              ( _blocksIlatex,_blocksIprocessors,_blocksIxhtml,_blocksIxml) =
                  (blocks_ _blocksOsem )
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml)))
sem_Block_Caption :: T_Inline  ->
                     T_Block  ->
                     T_Block 
sem_Block_Caption caption_ contents_  =
    (\ _lhsIsem ->
         (let _lhsOxhtml :: Node
              _lhsOxml :: (XL.Content)
              _lhsOlatex :: ShowS
              _lhsOprocessors :: ([Processor Document Syn_Document])
              _contentsOsem :: Document
              _captionIlatex :: ShowS
              _captionIxhtml :: Node
              _captionIxml :: (XL.Content)
              _contentsIlatex :: ShowS
              _contentsIprocessors :: ([Processor Document Syn_Document])
              _contentsIxhtml :: Node
              _contentsIxml :: (XL.Content)
              -- "src/Text/Document/Printer/Xhtml.ag"(line 91, column 16)
              _lhsOxhtml =
                  classDiv "caption" $ nodeset [(classDiv "caption-label" _captionIxhtml), _contentsIxhtml]
              -- "src/Text/Document/Printer/Xml.ag"(line 89, column 16)
              _lhsOxml =
                  xmlElem "caption"   [_captionIxml, _contentsIxml]
              -- "src/Text/Document/Printer/Latex.ag"(line 109, column 16)
              _lhsOlatex =
                  ss "-- todo Caption --"
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  _contentsIprocessors
              -- copy rule (down)
              _contentsOsem =
                  _lhsIsem
              ( _captionIlatex,_captionIxhtml,_captionIxml) =
                  (caption_ )
              ( _contentsIlatex,_contentsIprocessors,_contentsIxhtml,_contentsIxml) =
                  (contents_ _contentsOsem )
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml)))
sem_Block_Comment :: String ->
                     T_Block 
sem_Block_Comment comment_  =
    (\ _lhsIsem ->
         (let _lhsOxhtml :: Node
              _lhsOxml :: (XL.Content)
              _lhsOlatex :: ShowS
              _lhsOprocessors :: ([Processor Document Syn_Document])
              -- "src/Text/Document/Printer/Xhtml.ag"(line 88, column 16)
              _lhsOxhtml =
                  H.text comment_
              -- "src/Text/Document/Printer/Xml.ag"(line 87, column 16)
              _lhsOxml =
                  xmlElem "comment"   [xmlText comment_]
              -- "src/Text/Document/Printer/Latex.ag"(line 111, column 16)
              _lhsOlatex =
                  ss comment_
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  []
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml)))
sem_Block_Enum :: T_Blocks  ->
                  T_Block 
sem_Block_Enum items_  =
    (\ _lhsIsem ->
         (let _lhsOxhtml :: Node
              _lhsOxml :: (XL.Content)
              _lhsOlatex :: ShowS
              _lhsOprocessors :: ([Processor Document Syn_Document])
              _itemsOsem :: Document
              _itemsIlatex :: ([ShowS])
              _itemsIprocessors :: ([Processor Document Syn_Document])
              _itemsIxhtml :: NodeSet
              _itemsIxml :: ([XL.Content])
              -- "src/Text/Document/Printer/Xhtml.ag"(line 84, column 16)
              _lhsOxhtml =
                  ol _itemsIxhtml
              -- "src/Text/Document/Printer/Xml.ag"(line 82, column 16)
              _lhsOxml =
                  xmlElem "enum"      (map (xmlElem "item" . (:[])) _itemsIxml)
              -- "src/Text/Document/Printer/Latex.ag"(line 104, column 16)
              _lhsOlatex =
                  texEnumerate $ concatMapS (texItem.)  _itemsIlatex
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  _itemsIprocessors
              -- copy rule (down)
              _itemsOsem =
                  _lhsIsem
              ( _itemsIlatex,_itemsIprocessors,_itemsIxhtml,_itemsIxml) =
                  (items_ _itemsOsem )
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml)))
sem_Block_Include :: String ->
                     T_Block 
sem_Block_Include document_  =
    (\ _lhsIsem ->
         (let _lhsOxhtml :: Node
              _lhsOxml :: (XL.Content)
              _lhsOlatex :: ShowS
              _lhsOprocessors :: ([Processor Document Syn_Document])
              -- "src/Text/Document/Printer/Xhtml.ag"(line 89, column 16)
              _lhsOxhtml =
                  classDiv "include" $ nodeset [H.text document_]
              -- "src/Text/Document/Printer/Xml.ag"(line 86, column 16)
              _lhsOxml =
                  xmlElem "include"   [xmlText document_]
              -- "src/Text/Document/Printer/Latex.ag"(line 110, column 16)
              _lhsOlatex =
                  ss "-- todo Include --"
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  []
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml)))
sem_Block_List :: T_Blocks  ->
                  T_Block 
sem_Block_List items_  =
    (\ _lhsIsem ->
         (let _lhsOxhtml :: Node
              _lhsOxml :: (XL.Content)
              _lhsOlatex :: ShowS
              _lhsOprocessors :: ([Processor Document Syn_Document])
              _itemsOsem :: Document
              _itemsIlatex :: ([ShowS])
              _itemsIprocessors :: ([Processor Document Syn_Document])
              _itemsIxhtml :: NodeSet
              _itemsIxml :: ([XL.Content])
              -- "src/Text/Document/Printer/Xhtml.ag"(line 85, column 16)
              _lhsOxhtml =
                  ul _itemsIxhtml
              -- "src/Text/Document/Printer/Xml.ag"(line 83, column 16)
              _lhsOxml =
                  xmlElem "list"      (map (xmlElem "item" . (:[])) _itemsIxml)
              -- "src/Text/Document/Printer/Latex.ag"(line 105, column 16)
              _lhsOlatex =
                  texItemize   $ concatMapS (texItem.)  _itemsIlatex
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  _itemsIprocessors
              -- copy rule (down)
              _itemsOsem =
                  _lhsIsem
              ( _itemsIlatex,_itemsIprocessors,_itemsIxhtml,_itemsIxml) =
                  (items_ _itemsOsem )
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml)))
sem_Block_Mapping :: T_Pairs  ->
                     T_Block 
sem_Block_Mapping pairs_  =
    (\ _lhsIsem ->
         (let _lhsOxhtml :: Node
              _lhsOxml :: (XL.Content)
              _lhsOlatex :: ShowS
              _lhsOprocessors :: ([Processor Document Syn_Document])
              _pairsOsem :: Document
              _pairsIlatex :: ([(ShowS, ShowS)])
              _pairsIxhtml :: ([(Node, Node)])
              _pairsIxml :: ([XL.Content])
              -- "src/Text/Document/Printer/Xhtml.ag"(line 86, column 16)
              _lhsOxhtml =
                  dl _pairsIxhtml
              -- "src/Text/Document/Printer/Xml.ag"(line 84, column 16)
              _lhsOxml =
                  xmlElem "mapping"   _pairsIxml
              -- "src/Text/Document/Printer/Latex.ag"(line 106, column 16)
              _lhsOlatex =
                  texItemize   $ concatMapS texMapping  _pairsIlatex
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  []
              -- copy rule (down)
              _pairsOsem =
                  _lhsIsem
              ( _pairsIlatex,_pairsIxhtml,_pairsIxml) =
                  (pairs_ _pairsOsem )
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml)))
sem_Block_Paragraph :: T_Inline  ->
                       T_Block 
sem_Block_Paragraph paragraph_  =
    (\ _lhsIsem ->
         (let _lhsOxhtml :: Node
              _lhsOxml :: (XL.Content)
              _lhsOlatex :: ShowS
              _lhsOprocessors :: ([Processor Document Syn_Document])
              _paragraphIlatex :: ShowS
              _paragraphIxhtml :: Node
              _paragraphIxml :: (XL.Content)
              -- "src/Text/Document/Printer/Xhtml.ag"(line 83, column 16)
              _lhsOxhtml =
                  par _paragraphIxhtml
              -- "src/Text/Document/Printer/Xml.ag"(line 81, column 16)
              _lhsOxml =
                  xmlElem "paragraph" [_paragraphIxml]
              -- "src/Text/Document/Printer/Latex.ag"(line 103, column 16)
              _lhsOlatex =
                  _paragraphIlatex
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  []
              ( _paragraphIlatex,_paragraphIxhtml,_paragraphIxml) =
                  (paragraph_ )
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml)))
sem_Block_Plugin :: ((PrettyPrinter Document, Processor Document Syn_Document)) ->
                    T_Block 
sem_Block_Plugin plugin_  =
    (\ _lhsIsem ->
         (let _lhsOxhtml :: Node
              _lhsOxml :: (XL.Content)
              _lhsOlatex :: ShowS
              _lhsOprocessors :: ([Processor Document Syn_Document])
              -- "src/Text/Document/Printer/Xhtml.ag"(line 92, column 16)
              _lhsOxhtml =
                  xhtmlPlugin (fst plugin_) _lhsIsem
              -- "src/Text/Document/Printer/Xml.ag"(line 90, column 16)
              _lhsOxml =
                  xmlElem "plugin"    [xmlPlugin (fst plugin_) _lhsIsem]
              -- "src/Text/Document/Printer/Latex.ag"(line 112, column 16)
              _lhsOlatex =
                  texPlugin (fst plugin_) _lhsIsem
              -- "src/Text/Document/Core/Processing.ag"(line 11, column 20)
              _lhsOprocessors =
                  [snd plugin_]
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml)))
sem_Block_Verbatim :: T_Block  ->
                      T_Block 
sem_Block_Verbatim verbatim_  =
    (\ _lhsIsem ->
         (let _lhsOxhtml :: Node
              _lhsOxml :: (XL.Content)
              _lhsOlatex :: ShowS
              _lhsOprocessors :: ([Processor Document Syn_Document])
              _verbatimOsem :: Document
              _verbatimIlatex :: ShowS
              _verbatimIprocessors :: ([Processor Document Syn_Document])
              _verbatimIxhtml :: Node
              _verbatimIxml :: (XL.Content)
              -- "src/Text/Document/Printer/Xhtml.ag"(line 87, column 16)
              _lhsOxhtml =
                  pre _verbatimIxhtml
              -- "src/Text/Document/Printer/Xml.ag"(line 85, column 16)
              _lhsOxml =
                  xmlElem "verbatim"  [_verbatimIxml]
              -- "src/Text/Document/Printer/Latex.ag"(line 107, column 16)
              _lhsOlatex =
                  ss "-- todo Verbatim --"
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  _verbatimIprocessors
              -- copy rule (down)
              _verbatimOsem =
                  _lhsIsem
              ( _verbatimIlatex,_verbatimIprocessors,_verbatimIxhtml,_verbatimIxml) =
                  (verbatim_ _verbatimOsem )
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml)))
-- Blocks ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         sem                  : Document 
      synthesized attributes:
         latex                : [ShowS]
         processors           : [Processor Document Syn_Document]
         xhtml                : NodeSet
         xml                  : [XL.Content]
   alternatives:
      alternative Cons:
         child hd             : Block 
         child tl             : Blocks 
      alternative Nil:
-}
type Blocks  = [(Block)]
-- cata
sem_Blocks :: Blocks  ->
              T_Blocks 
sem_Blocks list  =
    (Prelude.foldr sem_Blocks_Cons sem_Blocks_Nil (Prelude.map sem_Block list) )
-- semantic domain
type T_Blocks  = Document ->
                 ( ([ShowS]),([Processor Document Syn_Document]),NodeSet,([XL.Content]))
data Inh_Blocks  = Inh_Blocks {sem_Inh_Blocks :: Document}
data Syn_Blocks  = Syn_Blocks {latex_Syn_Blocks :: [ShowS],processors_Syn_Blocks :: [Processor Document Syn_Document],xhtml_Syn_Blocks :: NodeSet,xml_Syn_Blocks :: [XL.Content]}
wrap_Blocks :: T_Blocks  ->
               Inh_Blocks  ->
               Syn_Blocks 
wrap_Blocks sem (Inh_Blocks _lhsIsem )  =
    (let ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml) =
             (sem _lhsIsem )
     in  (Syn_Blocks _lhsOlatex _lhsOprocessors _lhsOxhtml _lhsOxml ))
sem_Blocks_Cons :: T_Block  ->
                   T_Blocks  ->
                   T_Blocks 
sem_Blocks_Cons hd_ tl_  =
    (\ _lhsIsem ->
         (let _lhsOlatex :: ([ShowS])
              _lhsOprocessors :: ([Processor Document Syn_Document])
              _lhsOxhtml :: NodeSet
              _lhsOxml :: ([XL.Content])
              _hdOsem :: Document
              _tlOsem :: Document
              _hdIlatex :: ShowS
              _hdIprocessors :: ([Processor Document Syn_Document])
              _hdIxhtml :: Node
              _hdIxml :: (XL.Content)
              _tlIlatex :: ([ShowS])
              _tlIprocessors :: ([Processor Document Syn_Document])
              _tlIxhtml :: NodeSet
              _tlIxml :: ([XL.Content])
              -- use rule "src/Text/Document/Printer/Latex.ag"(line 75, column 39)
              _lhsOlatex =
                  _hdIlatex : _tlIlatex
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  _hdIprocessors ++ _tlIprocessors
              -- use rule "src/Text/Document/Printer/Xhtml.ag"(line 58, column 39)
              _lhsOxhtml =
                  _hdIxhtml : _tlIxhtml
              -- use rule "src/Text/Document/Printer/Xml.ag"(line 45, column 13)
              _lhsOxml =
                  _hdIxml : _tlIxml
              -- copy rule (down)
              _hdOsem =
                  _lhsIsem
              -- copy rule (down)
              _tlOsem =
                  _lhsIsem
              ( _hdIlatex,_hdIprocessors,_hdIxhtml,_hdIxml) =
                  (hd_ _hdOsem )
              ( _tlIlatex,_tlIprocessors,_tlIxhtml,_tlIxml) =
                  (tl_ _tlOsem )
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml)))
sem_Blocks_Nil :: T_Blocks 
sem_Blocks_Nil  =
    (\ _lhsIsem ->
         (let _lhsOlatex :: ([ShowS])
              _lhsOprocessors :: ([Processor Document Syn_Document])
              _lhsOxhtml :: NodeSet
              _lhsOxml :: ([XL.Content])
              -- use rule "src/Text/Document/Printer/Latex.ag"(line 75, column 39)
              _lhsOlatex =
                  []
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  []
              -- use rule "src/Text/Document/Printer/Xhtml.ag"(line 58, column 39)
              _lhsOxhtml =
                  []
              -- use rule "src/Text/Document/Printer/Xml.ag"(line 45, column 13)
              _lhsOxml =
                  []
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml)))
-- Document ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         sem                  : Document 
      synthesized attributes:
         latex                : ShowS
         processors           : [Processor Document Syn_Document]
         xhtml                : Node
         xhtmlTOC             : NodeSet
         xml                  : XL.Content
   alternatives:
      alternative Document:
         child title          : Title 
         child subtitle       : Title 
         child annotations    : Annotations 
         child abstract       : MaybeBlock 
         child body           : SectionContents 
-}
data Document  = Document_Document (Title) (Title) (Annotations) (MaybeBlock) (SectionContents) 
               deriving ( Eq,Show)
-- cata
sem_Document :: Document  ->
                T_Document 
sem_Document (Document_Document _title _subtitle _annotations _abstract _body )  =
    (sem_Document_Document (sem_Title _title ) (sem_Title _subtitle ) (sem_Annotations _annotations ) (sem_MaybeBlock _abstract ) (sem_SectionContents _body ) )
-- semantic domain
type T_Document  = Document ->
                   ( ShowS,([Processor Document Syn_Document]),Node,NodeSet,(XL.Content))
data Inh_Document  = Inh_Document {sem_Inh_Document :: Document}
data Syn_Document  = Syn_Document {latex_Syn_Document :: ShowS,processors_Syn_Document :: [Processor Document Syn_Document],xhtml_Syn_Document :: Node,xhtmlTOC_Syn_Document :: NodeSet,xml_Syn_Document :: XL.Content}
wrap_Document :: T_Document  ->
                 Inh_Document  ->
                 Syn_Document 
wrap_Document sem (Inh_Document _lhsIsem )  =
    (let ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxhtmlTOC,_lhsOxml) =
             (sem _lhsIsem )
     in  (Syn_Document _lhsOlatex _lhsOprocessors _lhsOxhtml _lhsOxhtmlTOC _lhsOxml ))
sem_Document_Document :: T_Title  ->
                         T_Title  ->
                         T_Annotations  ->
                         T_MaybeBlock  ->
                         T_SectionContents  ->
                         T_Document 
sem_Document_Document title_ subtitle_ annotations_ abstract_ body_  =
    (\ _lhsIsem ->
         (let _bodyOlevel :: Int
              _bodyOsection :: ([Int])
              _lhsOxhtml :: Node
              _lhsOxml :: (XL.Content)
              _lhsOlatex :: ShowS
              _lhsOxhtmlTOC :: NodeSet
              _lhsOprocessors :: ([Processor Document Syn_Document])
              _abstractOsem :: Document
              _bodyOsem :: Document
              _titleIisTitle :: Bool
              _titleIlatex :: ShowS
              _titleIxhtml :: Node
              _titleIxml :: ([XL.Content])
              _subtitleIisTitle :: Bool
              _subtitleIlatex :: ShowS
              _subtitleIxhtml :: Node
              _subtitleIxml :: ([XL.Content])
              _annotationsIxhtml :: ([(String, Node)])
              _annotationsIxml :: ([XL.Content])
              _abstractIlatex :: ShowS
              _abstractIprocessors :: ([Processor Document Syn_Document])
              _abstractIxhtml :: Node
              _abstractIxml :: ([XL.Content])
              _bodyIlatex :: ([ShowS])
              _bodyIprocessors :: ([Processor Document Syn_Document])
              _bodyIsection :: ([Int])
              _bodyIxhtml :: NodeSet
              _bodyIxhtmlTOC :: NodeSet
              _bodyIxml :: ([XL.Content])
              -- "src/Text/Document/Core/Structure.ag"(line 10, column 15)
              _bodyOlevel =
                  1
              -- "src/Text/Document/Core/Structure.ag"(line 33, column 14)
              _bodyOsection =
                  [1]
              -- "src/Text/Document/Printer/Xhtml.ag"(line 65, column 16)
              _lhsOxhtml =
                  xhtmlDocument
                  _titleIxhtml
                  _subtitleIxhtml
                  _annotationsIxhtml
                  _abstractIxhtml
                  _bodyIxhtml
              -- "src/Text/Document/Printer/Xml.ag"(line 48, column 16)
              _lhsOxml =
                  xmlElem "document" [
                    xmlElem "title"       (map xmlStrip _titleIxml)
                  , xmlElem "subtitle"    (map xmlStrip _subtitleIxml)
                  , xmlElem "annotations" _annotationsIxml
                  , xmlElem "abstract"    _abstractIxml
                  , xmlElem "body"        _bodyIxml
                  ]
              -- "src/Text/Document/Printer/Latex.ag"(line 82, column 16)
              _lhsOlatex =
                  texDocument
                  _titleIlatex
                  _subtitleIlatex
                  _abstractIlatex
                  (unlinesS _bodyIlatex)
              -- "src/Text/Document/Plugin/TOC.ag"(line 12, column 14)
              _lhsOxhtmlTOC =
                  xhtmlCleanList _bodyIxhtmlTOC
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  _abstractIprocessors ++ _bodyIprocessors
              -- copy rule (down)
              _abstractOsem =
                  _lhsIsem
              -- copy rule (down)
              _bodyOsem =
                  _lhsIsem
              ( _titleIisTitle,_titleIlatex,_titleIxhtml,_titleIxml) =
                  (title_ )
              ( _subtitleIisTitle,_subtitleIlatex,_subtitleIxhtml,_subtitleIxml) =
                  (subtitle_ )
              ( _annotationsIxhtml,_annotationsIxml) =
                  (annotations_ )
              ( _abstractIlatex,_abstractIprocessors,_abstractIxhtml,_abstractIxml) =
                  (abstract_ _abstractOsem )
              ( _bodyIlatex,_bodyIprocessors,_bodyIsection,_bodyIxhtml,_bodyIxhtmlTOC,_bodyIxml) =
                  (body_ _bodyOlevel _bodyOsection _bodyOsem )
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxhtmlTOC,_lhsOxml)))
-- EitherStructureBlock ----------------------------------------
{-
   visit 0:
      inherited attributes:
         level                : Int
         sem                  : Document 
      chained attribute:
         section              : [Int]
      synthesized attributes:
         latex                : ShowS
         processors           : [Processor Document Syn_Document]
         xhtml                : Node
         xhtmlTOC             : NodeSet
         xml                  : XL.Content
   alternatives:
      alternative Left:
         child left           : Structure 
      alternative Right:
         child right          : Block 
-}
type EitherStructureBlock  = (Either (Structure) (Block))
-- cata
sem_EitherStructureBlock :: EitherStructureBlock  ->
                            T_EitherStructureBlock 
sem_EitherStructureBlock (Prelude.Left x )  =
    (sem_EitherStructureBlock_Left (sem_Structure x ) )
sem_EitherStructureBlock (Prelude.Right x )  =
    (sem_EitherStructureBlock_Right (sem_Block x ) )
-- semantic domain
type T_EitherStructureBlock  = Int ->
                               ([Int]) ->
                               Document ->
                               ( ShowS,([Processor Document Syn_Document]),([Int]),Node,NodeSet,(XL.Content))
data Inh_EitherStructureBlock  = Inh_EitherStructureBlock {level_Inh_EitherStructureBlock :: Int,section_Inh_EitherStructureBlock :: [Int],sem_Inh_EitherStructureBlock :: Document}
data Syn_EitherStructureBlock  = Syn_EitherStructureBlock {latex_Syn_EitherStructureBlock :: ShowS,processors_Syn_EitherStructureBlock :: [Processor Document Syn_Document],section_Syn_EitherStructureBlock :: [Int],xhtml_Syn_EitherStructureBlock :: Node,xhtmlTOC_Syn_EitherStructureBlock :: NodeSet,xml_Syn_EitherStructureBlock :: XL.Content}
wrap_EitherStructureBlock :: T_EitherStructureBlock  ->
                             Inh_EitherStructureBlock  ->
                             Syn_EitherStructureBlock 
wrap_EitherStructureBlock sem (Inh_EitherStructureBlock _lhsIlevel _lhsIsection _lhsIsem )  =
    (let ( _lhsOlatex,_lhsOprocessors,_lhsOsection,_lhsOxhtml,_lhsOxhtmlTOC,_lhsOxml) =
             (sem _lhsIlevel _lhsIsection _lhsIsem )
     in  (Syn_EitherStructureBlock _lhsOlatex _lhsOprocessors _lhsOsection _lhsOxhtml _lhsOxhtmlTOC _lhsOxml ))
sem_EitherStructureBlock_Left :: T_Structure  ->
                                 T_EitherStructureBlock 
sem_EitherStructureBlock_Left left_  =
    (\ _lhsIlevel
       _lhsIsection
       _lhsIsem ->
         (let _lhsOlatex :: ShowS
              _lhsOprocessors :: ([Processor Document Syn_Document])
              _lhsOxhtml :: Node
              _lhsOxhtmlTOC :: NodeSet
              _lhsOsection :: ([Int])
              _lhsOxml :: (XL.Content)
              _leftOlevel :: Int
              _leftOsection :: ([Int])
              _leftOsem :: Document
              _leftIlatex :: ShowS
              _leftIprocessors :: ([Processor Document Syn_Document])
              _leftIsection :: ([Int])
              _leftIxhtml :: Node
              _leftIxhtmlTOC :: NodeSet
              _leftIxml :: (XL.Content)
              -- use rule "src/Text/Document/Printer/Latex.ag"(line 73, column 39)
              _lhsOlatex =
                  _leftIlatex
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  _leftIprocessors
              -- use rule "src/Text/Document/Printer/Xhtml.ag"(line 56, column 39)
              _lhsOxhtml =
                  _leftIxhtml
              -- use rule "src/Text/Document/Plugin/TOC.ag"(line 8, column 52)
              _lhsOxhtmlTOC =
                  _leftIxhtmlTOC
              -- copy rule (up)
              _lhsOsection =
                  _leftIsection
              -- copy rule (up)
              _lhsOxml =
                  _leftIxml
              -- copy rule (down)
              _leftOlevel =
                  _lhsIlevel
              -- copy rule (down)
              _leftOsection =
                  _lhsIsection
              -- copy rule (down)
              _leftOsem =
                  _lhsIsem
              ( _leftIlatex,_leftIprocessors,_leftIsection,_leftIxhtml,_leftIxhtmlTOC,_leftIxml) =
                  (left_ _leftOlevel _leftOsection _leftOsem )
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOsection,_lhsOxhtml,_lhsOxhtmlTOC,_lhsOxml)))
sem_EitherStructureBlock_Right :: T_Block  ->
                                  T_EitherStructureBlock 
sem_EitherStructureBlock_Right right_  =
    (\ _lhsIlevel
       _lhsIsection
       _lhsIsem ->
         (let _lhsOlatex :: ShowS
              _lhsOprocessors :: ([Processor Document Syn_Document])
              _lhsOxhtml :: Node
              _lhsOxhtmlTOC :: NodeSet
              _lhsOsection :: ([Int])
              _lhsOxml :: (XL.Content)
              _rightOsem :: Document
              _rightIlatex :: ShowS
              _rightIprocessors :: ([Processor Document Syn_Document])
              _rightIxhtml :: Node
              _rightIxml :: (XL.Content)
              -- use rule "src/Text/Document/Printer/Latex.ag"(line 73, column 39)
              _lhsOlatex =
                  _rightIlatex
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  _rightIprocessors
              -- use rule "src/Text/Document/Printer/Xhtml.ag"(line 56, column 39)
              _lhsOxhtml =
                  _rightIxhtml
              -- use rule "src/Text/Document/Plugin/TOC.ag"(line 8, column 52)
              _lhsOxhtmlTOC =
                  []
              -- copy rule (chain)
              _lhsOsection =
                  _lhsIsection
              -- copy rule (up)
              _lhsOxml =
                  _rightIxml
              -- copy rule (down)
              _rightOsem =
                  _lhsIsem
              ( _rightIlatex,_rightIprocessors,_rightIxhtml,_rightIxml) =
                  (right_ _rightOsem )
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOsection,_lhsOxhtml,_lhsOxhtmlTOC,_lhsOxml)))
-- Inline ------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         latex                : ShowS
         xhtml                : Node
         xml                  : XL.Content
   alternatives:
      alternative Emph:
         child emph           : Inline 
      alternative Fixed:
         child fixed          : Inline 
      alternative Footnote:
         child note           : Inline 
      alternative Image:
         child source         : {String}
         child description    : {String}
      alternative Lines:
         child lines          : Inlines 
      alternative Link:
         child ref            : {String}
         child kind           : {LinkType}
         child description    : Inline 
      alternative Quote:
         child quote          : Inline 
      alternative Strong:
         child strong         : Inline 
      alternative Text:
         child text           : {String}
-}
data Inline  = Inline_Emph (Inline) 
             | Inline_Fixed (Inline) 
             | Inline_Footnote (Inline) 
             | Inline_Image (String) (String) 
             | Inline_Lines (Inlines) 
             | Inline_Link (String) (LinkType) (Inline) 
             | Inline_Quote (Inline) 
             | Inline_Strong (Inline) 
             | Inline_Text (String) 
             deriving ( Eq,Show)
-- cata
sem_Inline :: Inline  ->
              T_Inline 
sem_Inline (Inline_Emph _emph )  =
    (sem_Inline_Emph (sem_Inline _emph ) )
sem_Inline (Inline_Fixed _fixed )  =
    (sem_Inline_Fixed (sem_Inline _fixed ) )
sem_Inline (Inline_Footnote _note )  =
    (sem_Inline_Footnote (sem_Inline _note ) )
sem_Inline (Inline_Image _source _description )  =
    (sem_Inline_Image _source _description )
sem_Inline (Inline_Lines _lines )  =
    (sem_Inline_Lines (sem_Inlines _lines ) )
sem_Inline (Inline_Link _ref _kind _description )  =
    (sem_Inline_Link _ref _kind (sem_Inline _description ) )
sem_Inline (Inline_Quote _quote )  =
    (sem_Inline_Quote (sem_Inline _quote ) )
sem_Inline (Inline_Strong _strong )  =
    (sem_Inline_Strong (sem_Inline _strong ) )
sem_Inline (Inline_Text _text )  =
    (sem_Inline_Text _text )
-- semantic domain
type T_Inline  = ( ShowS,Node,(XL.Content))
data Inh_Inline  = Inh_Inline {}
data Syn_Inline  = Syn_Inline {latex_Syn_Inline :: ShowS,xhtml_Syn_Inline :: Node,xml_Syn_Inline :: XL.Content}
wrap_Inline :: T_Inline  ->
               Inh_Inline  ->
               Syn_Inline 
wrap_Inline sem (Inh_Inline )  =
    (let ( _lhsOlatex,_lhsOxhtml,_lhsOxml) =
             (sem )
     in  (Syn_Inline _lhsOlatex _lhsOxhtml _lhsOxml ))
sem_Inline_Emph :: T_Inline  ->
                   T_Inline 
sem_Inline_Emph emph_  =
    (let _lhsOxhtml :: Node
         _lhsOxml :: (XL.Content)
         _lhsOlatex :: ShowS
         _emphIlatex :: ShowS
         _emphIxhtml :: Node
         _emphIxml :: (XL.Content)
         -- "src/Text/Document/Printer/Xhtml.ag"(line 98, column 16)
         _lhsOxhtml =
             H.emph _emphIxhtml
         -- "src/Text/Document/Printer/Xml.ag"(line 99, column 16)
         _lhsOxml =
             xmlElem "emph"   [_emphIxml]
         -- "src/Text/Document/Printer/Latex.ag"(line 118, column 16)
         _lhsOlatex =
             texEmph _emphIlatex
         ( _emphIlatex,_emphIxhtml,_emphIxml) =
             (emph_ )
     in  ( _lhsOlatex,_lhsOxhtml,_lhsOxml))
sem_Inline_Fixed :: T_Inline  ->
                    T_Inline 
sem_Inline_Fixed fixed_  =
    (let _lhsOxhtml :: Node
         _lhsOxml :: (XL.Content)
         _lhsOlatex :: ShowS
         _fixedIlatex :: ShowS
         _fixedIxhtml :: Node
         _fixedIxml :: (XL.Content)
         -- "src/Text/Document/Printer/Xhtml.ag"(line 100, column 16)
         _lhsOxhtml =
             tt _fixedIxhtml
         -- "src/Text/Document/Printer/Xml.ag"(line 101, column 16)
         _lhsOxml =
             xmlElem "fixed"  [_fixedIxml]
         -- "src/Text/Document/Printer/Latex.ag"(line 120, column 16)
         _lhsOlatex =
             texFixed _fixedIlatex
         ( _fixedIlatex,_fixedIxhtml,_fixedIxml) =
             (fixed_ )
     in  ( _lhsOlatex,_lhsOxhtml,_lhsOxml))
sem_Inline_Footnote :: T_Inline  ->
                       T_Inline 
sem_Inline_Footnote note_  =
    (let _lhsOxhtml :: Node
         _lhsOxml :: (XL.Content)
         _lhsOlatex :: ShowS
         _noteIlatex :: ShowS
         _noteIxhtml :: Node
         _noteIxml :: (XL.Content)
         -- "src/Text/Document/Printer/Xhtml.ag"(line 102, column 16)
         _lhsOxhtml =
             classSpan "footnote" _noteIxhtml
         -- "src/Text/Document/Printer/Xml.ag"(line 107, column 16)
         _lhsOxml =
             xmlElem "footnote" [_noteIxml]
         -- "src/Text/Document/Printer/Latex.ag"(line 124, column 16)
         _lhsOlatex =
             texFootnote _noteIlatex
         ( _noteIlatex,_noteIxhtml,_noteIxml) =
             (note_ )
     in  ( _lhsOlatex,_lhsOxhtml,_lhsOxml))
sem_Inline_Image :: String ->
                    String ->
                    T_Inline 
sem_Inline_Image source_ description_  =
    (let _lhsOxhtml :: Node
         _lhsOxml :: (XL.Content)
         _lhsOlatex :: ShowS
         -- "src/Text/Document/Printer/Xhtml.ag"(line 96, column 16)
         _lhsOxhtml =
             img source_ description_
         -- "src/Text/Document/Printer/Xml.ag"(line 94, column 16)
         _lhsOxml =
             xmlElem "image" [
               xmlElem "source"      [xmlText source_]
             , xmlElem "description" [xmlText description_]
             ]
         -- "src/Text/Document/Printer/Latex.ag"(line 116, column 16)
         _lhsOlatex =
             ss "todo"
     in  ( _lhsOlatex,_lhsOxhtml,_lhsOxml))
sem_Inline_Lines :: T_Inlines  ->
                    T_Inline 
sem_Inline_Lines lines_  =
    (let _lhsOxhtml :: Node
         _lhsOxml :: (XL.Content)
         _lhsOlatex :: ShowS
         _linesIlatex :: ([ShowS])
         _linesIxhtml :: NodeSet
         _linesIxml :: ([XL.Content])
         -- "src/Text/Document/Printer/Xhtml.ag"(line 95, column 16)
         _lhsOxhtml =
             nodeset _linesIxhtml
         -- "src/Text/Document/Printer/Xml.ag"(line 93, column 16)
         _lhsOxml =
             xmlElem "lines" _linesIxml
         -- "src/Text/Document/Printer/Latex.ag"(line 115, column 16)
         _lhsOlatex =
             concatS _linesIlatex
         ( _linesIlatex,_linesIxhtml,_linesIxml) =
             (lines_ )
     in  ( _lhsOlatex,_lhsOxhtml,_lhsOxml))
sem_Inline_Link :: String ->
                   LinkType ->
                   T_Inline  ->
                   T_Inline 
sem_Inline_Link ref_ kind_ description_  =
    (let _lhsOxhtml :: Node
         _lhsOxml :: (XL.Content)
         _lhsOlatex :: ShowS
         _descriptionIlatex :: ShowS
         _descriptionIxhtml :: Node
         _descriptionIxml :: (XL.Content)
         -- "src/Text/Document/Printer/Xhtml.ag"(line 101, column 16)
         _lhsOxhtml =
             xhtmlLink kind_ (linkAnchor kind_ ref_) _descriptionIxhtml
         -- "src/Text/Document/Printer/Xml.ag"(line 102, column 16)
         _lhsOxml =
             xmlElem "link" [
               xmlElem "ref"         [xmlText ref_]
             , xmlElem "kind"        [xmlText (show kind_)]
             , xmlElem "description" [_descriptionIxml]
             ]
         -- "src/Text/Document/Printer/Latex.ag"(line 121, column 16)
         _lhsOlatex =
             if kind_ == External
             then texLink ref_ _descriptionIlatex
             else ss "-- todo non-External Links --"
         ( _descriptionIlatex,_descriptionIxhtml,_descriptionIxml) =
             (description_ )
     in  ( _lhsOlatex,_lhsOxhtml,_lhsOxml))
sem_Inline_Quote :: T_Inline  ->
                    T_Inline 
sem_Inline_Quote quote_  =
    (let _lhsOxhtml :: Node
         _lhsOxml :: (XL.Content)
         _lhsOlatex :: ShowS
         _quoteIlatex :: ShowS
         _quoteIxhtml :: Node
         _quoteIxml :: (XL.Content)
         -- "src/Text/Document/Printer/Xhtml.ag"(line 103, column 16)
         _lhsOxhtml =
             cite _quoteIxhtml
         -- "src/Text/Document/Printer/Xml.ag"(line 108, column 16)
         _lhsOxml =
             xmlElem "quote"    [_quoteIxml]
         -- "src/Text/Document/Printer/Latex.ag"(line 125, column 16)
         _lhsOlatex =
             ss "-- todo Quote --"
         ( _quoteIlatex,_quoteIxhtml,_quoteIxml) =
             (quote_ )
     in  ( _lhsOlatex,_lhsOxhtml,_lhsOxml))
sem_Inline_Strong :: T_Inline  ->
                     T_Inline 
sem_Inline_Strong strong_  =
    (let _lhsOxhtml :: Node
         _lhsOxml :: (XL.Content)
         _lhsOlatex :: ShowS
         _strongIlatex :: ShowS
         _strongIxhtml :: Node
         _strongIxml :: (XL.Content)
         -- "src/Text/Document/Printer/Xhtml.ag"(line 99, column 16)
         _lhsOxhtml =
             H.strong _strongIxhtml
         -- "src/Text/Document/Printer/Xml.ag"(line 100, column 16)
         _lhsOxml =
             xmlElem "strong" [_strongIxml]
         -- "src/Text/Document/Printer/Latex.ag"(line 119, column 16)
         _lhsOlatex =
             texStrong _strongIlatex
         ( _strongIlatex,_strongIxhtml,_strongIxml) =
             (strong_ )
     in  ( _lhsOlatex,_lhsOxhtml,_lhsOxml))
sem_Inline_Text :: String ->
                   T_Inline 
sem_Inline_Text text_  =
    (let _lhsOxhtml :: Node
         _lhsOxml :: (XL.Content)
         _lhsOlatex :: ShowS
         -- "src/Text/Document/Printer/Xhtml.ag"(line 97, column 16)
         _lhsOxhtml =
             H.text text_
         -- "src/Text/Document/Printer/Xml.ag"(line 98, column 16)
         _lhsOxml =
             xmlText text_
         -- "src/Text/Document/Printer/Latex.ag"(line 117, column 16)
         _lhsOlatex =
             ss (escapeTex text_)
     in  ( _lhsOlatex,_lhsOxhtml,_lhsOxml))
-- Inlines -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         latex                : [ShowS]
         xhtml                : NodeSet
         xml                  : [XL.Content]
   alternatives:
      alternative Cons:
         child hd             : Inline 
         child tl             : Inlines 
      alternative Nil:
-}
type Inlines  = [(Inline)]
-- cata
sem_Inlines :: Inlines  ->
               T_Inlines 
sem_Inlines list  =
    (Prelude.foldr sem_Inlines_Cons sem_Inlines_Nil (Prelude.map sem_Inline list) )
-- semantic domain
type T_Inlines  = ( ([ShowS]),NodeSet,([XL.Content]))
data Inh_Inlines  = Inh_Inlines {}
data Syn_Inlines  = Syn_Inlines {latex_Syn_Inlines :: [ShowS],xhtml_Syn_Inlines :: NodeSet,xml_Syn_Inlines :: [XL.Content]}
wrap_Inlines :: T_Inlines  ->
                Inh_Inlines  ->
                Syn_Inlines 
wrap_Inlines sem (Inh_Inlines )  =
    (let ( _lhsOlatex,_lhsOxhtml,_lhsOxml) =
             (sem )
     in  (Syn_Inlines _lhsOlatex _lhsOxhtml _lhsOxml ))
sem_Inlines_Cons :: T_Inline  ->
                    T_Inlines  ->
                    T_Inlines 
sem_Inlines_Cons hd_ tl_  =
    (let _lhsOlatex :: ([ShowS])
         _lhsOxhtml :: NodeSet
         _lhsOxml :: ([XL.Content])
         _hdIlatex :: ShowS
         _hdIxhtml :: Node
         _hdIxml :: (XL.Content)
         _tlIlatex :: ([ShowS])
         _tlIxhtml :: NodeSet
         _tlIxml :: ([XL.Content])
         -- use rule "src/Text/Document/Printer/Latex.ag"(line 75, column 39)
         _lhsOlatex =
             _hdIlatex : _tlIlatex
         -- use rule "src/Text/Document/Printer/Xhtml.ag"(line 58, column 39)
         _lhsOxhtml =
             _hdIxhtml : _tlIxhtml
         -- use rule "src/Text/Document/Printer/Xml.ag"(line 45, column 13)
         _lhsOxml =
             _hdIxml : _tlIxml
         ( _hdIlatex,_hdIxhtml,_hdIxml) =
             (hd_ )
         ( _tlIlatex,_tlIxhtml,_tlIxml) =
             (tl_ )
     in  ( _lhsOlatex,_lhsOxhtml,_lhsOxml))
sem_Inlines_Nil :: T_Inlines 
sem_Inlines_Nil  =
    (let _lhsOlatex :: ([ShowS])
         _lhsOxhtml :: NodeSet
         _lhsOxml :: ([XL.Content])
         -- use rule "src/Text/Document/Printer/Latex.ag"(line 75, column 39)
         _lhsOlatex =
             []
         -- use rule "src/Text/Document/Printer/Xhtml.ag"(line 58, column 39)
         _lhsOxhtml =
             []
         -- use rule "src/Text/Document/Printer/Xml.ag"(line 45, column 13)
         _lhsOxml =
             []
     in  ( _lhsOlatex,_lhsOxhtml,_lhsOxml))
-- MaybeBlock --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         sem                  : Document 
      synthesized attributes:
         latex                : ShowS
         processors           : [Processor Document Syn_Document]
         xhtml                : Node
         xml                  : [XL.Content]
   alternatives:
      alternative Just:
         child just           : Block 
      alternative Nothing:
-}
type MaybeBlock  = (Maybe (Block))
-- cata
sem_MaybeBlock :: MaybeBlock  ->
                  T_MaybeBlock 
sem_MaybeBlock (Prelude.Just x )  =
    (sem_MaybeBlock_Just (sem_Block x ) )
sem_MaybeBlock Prelude.Nothing  =
    sem_MaybeBlock_Nothing
-- semantic domain
type T_MaybeBlock  = Document ->
                     ( ShowS,([Processor Document Syn_Document]),Node,([XL.Content]))
data Inh_MaybeBlock  = Inh_MaybeBlock {sem_Inh_MaybeBlock :: Document}
data Syn_MaybeBlock  = Syn_MaybeBlock {latex_Syn_MaybeBlock :: ShowS,processors_Syn_MaybeBlock :: [Processor Document Syn_Document],xhtml_Syn_MaybeBlock :: Node,xml_Syn_MaybeBlock :: [XL.Content]}
wrap_MaybeBlock :: T_MaybeBlock  ->
                   Inh_MaybeBlock  ->
                   Syn_MaybeBlock 
wrap_MaybeBlock sem (Inh_MaybeBlock _lhsIsem )  =
    (let ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml) =
             (sem _lhsIsem )
     in  (Syn_MaybeBlock _lhsOlatex _lhsOprocessors _lhsOxhtml _lhsOxml ))
sem_MaybeBlock_Just :: T_Block  ->
                       T_MaybeBlock 
sem_MaybeBlock_Just just_  =
    (\ _lhsIsem ->
         (let _lhsOxml :: ([XL.Content])
              _lhsOlatex :: ShowS
              _lhsOprocessors :: ([Processor Document Syn_Document])
              _lhsOxhtml :: Node
              _justOsem :: Document
              _justIlatex :: ShowS
              _justIprocessors :: ([Processor Document Syn_Document])
              _justIxhtml :: Node
              _justIxml :: (XL.Content)
              -- "src/Text/Document/Printer/Xml.ag"(line 68, column 16)
              _lhsOxml =
                  [xmlElem "maybeblock" [_justIxml]]
              -- use rule "src/Text/Document/Printer/Latex.ag"(line 73, column 39)
              _lhsOlatex =
                  _justIlatex
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  _justIprocessors
              -- use rule "src/Text/Document/Printer/Xhtml.ag"(line 56, column 39)
              _lhsOxhtml =
                  _justIxhtml
              -- copy rule (down)
              _justOsem =
                  _lhsIsem
              ( _justIlatex,_justIprocessors,_justIxhtml,_justIxml) =
                  (just_ _justOsem )
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml)))
sem_MaybeBlock_Nothing :: T_MaybeBlock 
sem_MaybeBlock_Nothing  =
    (\ _lhsIsem ->
         (let _lhsOxml :: ([XL.Content])
              _lhsOlatex :: ShowS
              _lhsOprocessors :: ([Processor Document Syn_Document])
              _lhsOxhtml :: Node
              -- "src/Text/Document/Printer/Xml.ag"(line 67, column 16)
              _lhsOxml =
                  []
              -- use rule "src/Text/Document/Printer/Latex.ag"(line 73, column 39)
              _lhsOlatex =
                  id
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  []
              -- use rule "src/Text/Document/Printer/Xhtml.ag"(line 56, column 39)
              _lhsOxhtml =
                  empty
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml)))
-- Pair --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         sem                  : Document 
      synthesized attributes:
         latex                : (ShowS, ShowS)
         processors           : [Processor Document Syn_Document]
         xhtml                : (Node, Node)
         xml                  : XL.Content
   alternatives:
      alternative Tuple:
         child x1             : Block 
         child x2             : Block 
-}
type Pair  = ( (Block),(Block))
-- cata
sem_Pair :: Pair  ->
            T_Pair 
sem_Pair ( x1,x2)  =
    (sem_Pair_Tuple (sem_Block x1 ) (sem_Block x2 ) )
-- semantic domain
type T_Pair  = Document ->
               ( ((ShowS, ShowS)),([Processor Document Syn_Document]),((Node, Node)),(XL.Content))
data Inh_Pair  = Inh_Pair {sem_Inh_Pair :: Document}
data Syn_Pair  = Syn_Pair {latex_Syn_Pair :: (ShowS, ShowS),processors_Syn_Pair :: [Processor Document Syn_Document],xhtml_Syn_Pair :: (Node, Node),xml_Syn_Pair :: XL.Content}
wrap_Pair :: T_Pair  ->
             Inh_Pair  ->
             Syn_Pair 
wrap_Pair sem (Inh_Pair _lhsIsem )  =
    (let ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml) =
             (sem _lhsIsem )
     in  (Syn_Pair _lhsOlatex _lhsOprocessors _lhsOxhtml _lhsOxml ))
sem_Pair_Tuple :: T_Block  ->
                  T_Block  ->
                  T_Pair 
sem_Pair_Tuple x1_ x2_  =
    (\ _lhsIsem ->
         (let _lhsOxhtml :: ((Node, Node))
              _lhsOxml :: (XL.Content)
              _lhsOlatex :: ((ShowS, ShowS))
              _lhsOprocessors :: ([Processor Document Syn_Document])
              _x1Osem :: Document
              _x2Osem :: Document
              _x1Ilatex :: ShowS
              _x1Iprocessors :: ([Processor Document Syn_Document])
              _x1Ixhtml :: Node
              _x1Ixml :: (XL.Content)
              _x2Ilatex :: ShowS
              _x2Iprocessors :: ([Processor Document Syn_Document])
              _x2Ixhtml :: Node
              _x2Ixml :: (XL.Content)
              -- "src/Text/Document/Printer/Xhtml.ag"(line 79, column 16)
              _lhsOxhtml =
                  (_x1Ixhtml, _x2Ixhtml)
              -- "src/Text/Document/Printer/Xml.ag"(line 77, column 16)
              _lhsOxml =
                  xmlElem "pair" [_x1Ixml, _x2Ixml]
              -- "src/Text/Document/Printer/Latex.ag"(line 99, column 16)
              _lhsOlatex =
                  (_x1Ilatex, _x2Ilatex)
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  _x1Iprocessors ++ _x2Iprocessors
              -- copy rule (down)
              _x1Osem =
                  _lhsIsem
              -- copy rule (down)
              _x2Osem =
                  _lhsIsem
              ( _x1Ilatex,_x1Iprocessors,_x1Ixhtml,_x1Ixml) =
                  (x1_ _x1Osem )
              ( _x2Ilatex,_x2Iprocessors,_x2Ixhtml,_x2Ixml) =
                  (x2_ _x2Osem )
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOxhtml,_lhsOxml)))
-- Pairs -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         sem                  : Document 
      synthesized attributes:
         latex                : [(ShowS, ShowS)]
         xhtml                : [(Node, Node)]
         xml                  : [XL.Content]
   alternatives:
      alternative Cons:
         child hd             : Pair 
         child tl             : Pairs 
      alternative Nil:
-}
type Pairs  = [(Pair)]
-- cata
sem_Pairs :: Pairs  ->
             T_Pairs 
sem_Pairs list  =
    (Prelude.foldr sem_Pairs_Cons sem_Pairs_Nil (Prelude.map sem_Pair list) )
-- semantic domain
type T_Pairs  = Document ->
                ( ([(ShowS, ShowS)]),([(Node, Node)]),([XL.Content]))
data Inh_Pairs  = Inh_Pairs {sem_Inh_Pairs :: Document}
data Syn_Pairs  = Syn_Pairs {latex_Syn_Pairs :: [(ShowS, ShowS)],xhtml_Syn_Pairs :: [(Node, Node)],xml_Syn_Pairs :: [XL.Content]}
wrap_Pairs :: T_Pairs  ->
              Inh_Pairs  ->
              Syn_Pairs 
wrap_Pairs sem (Inh_Pairs _lhsIsem )  =
    (let ( _lhsOlatex,_lhsOxhtml,_lhsOxml) =
             (sem _lhsIsem )
     in  (Syn_Pairs _lhsOlatex _lhsOxhtml _lhsOxml ))
sem_Pairs_Cons :: T_Pair  ->
                  T_Pairs  ->
                  T_Pairs 
sem_Pairs_Cons hd_ tl_  =
    (\ _lhsIsem ->
         (let _lhsOlatex :: ([(ShowS, ShowS)])
              _lhsOxhtml :: ([(Node, Node)])
              _lhsOxml :: ([XL.Content])
              _hdOsem :: Document
              _tlOsem :: Document
              _hdIlatex :: ((ShowS, ShowS))
              _hdIprocessors :: ([Processor Document Syn_Document])
              _hdIxhtml :: ((Node, Node))
              _hdIxml :: (XL.Content)
              _tlIlatex :: ([(ShowS, ShowS)])
              _tlIxhtml :: ([(Node, Node)])
              _tlIxml :: ([XL.Content])
              -- use rule "src/Text/Document/Printer/Latex.ag"(line 78, column 39)
              _lhsOlatex =
                  _hdIlatex : _tlIlatex
              -- use rule "src/Text/Document/Printer/Xhtml.ag"(line 61, column 39)
              _lhsOxhtml =
                  _hdIxhtml : _tlIxhtml
              -- use rule "src/Text/Document/Printer/Xml.ag"(line 45, column 13)
              _lhsOxml =
                  _hdIxml : _tlIxml
              -- copy rule (down)
              _hdOsem =
                  _lhsIsem
              -- copy rule (down)
              _tlOsem =
                  _lhsIsem
              ( _hdIlatex,_hdIprocessors,_hdIxhtml,_hdIxml) =
                  (hd_ _hdOsem )
              ( _tlIlatex,_tlIxhtml,_tlIxml) =
                  (tl_ _tlOsem )
          in  ( _lhsOlatex,_lhsOxhtml,_lhsOxml)))
sem_Pairs_Nil :: T_Pairs 
sem_Pairs_Nil  =
    (\ _lhsIsem ->
         (let _lhsOlatex :: ([(ShowS, ShowS)])
              _lhsOxhtml :: ([(Node, Node)])
              _lhsOxml :: ([XL.Content])
              -- use rule "src/Text/Document/Printer/Latex.ag"(line 78, column 39)
              _lhsOlatex =
                  []
              -- use rule "src/Text/Document/Printer/Xhtml.ag"(line 61, column 39)
              _lhsOxhtml =
                  []
              -- use rule "src/Text/Document/Printer/Xml.ag"(line 45, column 13)
              _lhsOxml =
                  []
          in  ( _lhsOlatex,_lhsOxhtml,_lhsOxml)))
-- SectionContents ---------------------------------------------
{-
   visit 0:
      inherited attributes:
         level                : Int
         sem                  : Document 
      chained attribute:
         section              : [Int]
      synthesized attributes:
         latex                : [ShowS]
         processors           : [Processor Document Syn_Document]
         xhtml                : NodeSet
         xhtmlTOC             : NodeSet
         xml                  : [XL.Content]
   alternatives:
      alternative Cons:
         child hd             : EitherStructureBlock 
         child tl             : SectionContents 
      alternative Nil:
-}
type SectionContents  = [(EitherStructureBlock)]
-- cata
sem_SectionContents :: SectionContents  ->
                       T_SectionContents 
sem_SectionContents list  =
    (Prelude.foldr sem_SectionContents_Cons sem_SectionContents_Nil (Prelude.map sem_EitherStructureBlock list) )
-- semantic domain
type T_SectionContents  = Int ->
                          ([Int]) ->
                          Document ->
                          ( ([ShowS]),([Processor Document Syn_Document]),([Int]),NodeSet,NodeSet,([XL.Content]))
data Inh_SectionContents  = Inh_SectionContents {level_Inh_SectionContents :: Int,section_Inh_SectionContents :: [Int],sem_Inh_SectionContents :: Document}
data Syn_SectionContents  = Syn_SectionContents {latex_Syn_SectionContents :: [ShowS],processors_Syn_SectionContents :: [Processor Document Syn_Document],section_Syn_SectionContents :: [Int],xhtml_Syn_SectionContents :: NodeSet,xhtmlTOC_Syn_SectionContents :: NodeSet,xml_Syn_SectionContents :: [XL.Content]}
wrap_SectionContents :: T_SectionContents  ->
                        Inh_SectionContents  ->
                        Syn_SectionContents 
wrap_SectionContents sem (Inh_SectionContents _lhsIlevel _lhsIsection _lhsIsem )  =
    (let ( _lhsOlatex,_lhsOprocessors,_lhsOsection,_lhsOxhtml,_lhsOxhtmlTOC,_lhsOxml) =
             (sem _lhsIlevel _lhsIsection _lhsIsem )
     in  (Syn_SectionContents _lhsOlatex _lhsOprocessors _lhsOsection _lhsOxhtml _lhsOxhtmlTOC _lhsOxml ))
sem_SectionContents_Cons :: T_EitherStructureBlock  ->
                            T_SectionContents  ->
                            T_SectionContents 
sem_SectionContents_Cons hd_ tl_  =
    (\ _lhsIlevel
       _lhsIsection
       _lhsIsem ->
         (let _lhsOxhtmlTOC :: NodeSet
              _lhsOlatex :: ([ShowS])
              _lhsOprocessors :: ([Processor Document Syn_Document])
              _lhsOxhtml :: NodeSet
              _lhsOxml :: ([XL.Content])
              _lhsOsection :: ([Int])
              _hdOlevel :: Int
              _hdOsection :: ([Int])
              _hdOsem :: Document
              _tlOlevel :: Int
              _tlOsection :: ([Int])
              _tlOsem :: Document
              _hdIlatex :: ShowS
              _hdIprocessors :: ([Processor Document Syn_Document])
              _hdIsection :: ([Int])
              _hdIxhtml :: Node
              _hdIxhtmlTOC :: NodeSet
              _hdIxml :: (XL.Content)
              _tlIlatex :: ([ShowS])
              _tlIprocessors :: ([Processor Document Syn_Document])
              _tlIsection :: ([Int])
              _tlIxhtml :: NodeSet
              _tlIxhtmlTOC :: NodeSet
              _tlIxml :: ([XL.Content])
              -- "src/Text/Document/Plugin/TOC.ag"(line 23, column 10)
              _lhsOxhtmlTOC =
                  _hdIxhtmlTOC ++ _tlIxhtmlTOC
              -- use rule "src/Text/Document/Printer/Latex.ag"(line 75, column 39)
              _lhsOlatex =
                  _hdIlatex : _tlIlatex
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  _hdIprocessors ++ _tlIprocessors
              -- use rule "src/Text/Document/Printer/Xhtml.ag"(line 58, column 39)
              _lhsOxhtml =
                  _hdIxhtml : _tlIxhtml
              -- use rule "src/Text/Document/Printer/Xml.ag"(line 45, column 13)
              _lhsOxml =
                  _hdIxml : _tlIxml
              -- copy rule (up)
              _lhsOsection =
                  _tlIsection
              -- copy rule (down)
              _hdOlevel =
                  _lhsIlevel
              -- copy rule (down)
              _hdOsection =
                  _lhsIsection
              -- copy rule (down)
              _hdOsem =
                  _lhsIsem
              -- copy rule (down)
              _tlOlevel =
                  _lhsIlevel
              -- copy rule (chain)
              _tlOsection =
                  _hdIsection
              -- copy rule (down)
              _tlOsem =
                  _lhsIsem
              ( _hdIlatex,_hdIprocessors,_hdIsection,_hdIxhtml,_hdIxhtmlTOC,_hdIxml) =
                  (hd_ _hdOlevel _hdOsection _hdOsem )
              ( _tlIlatex,_tlIprocessors,_tlIsection,_tlIxhtml,_tlIxhtmlTOC,_tlIxml) =
                  (tl_ _tlOlevel _tlOsection _tlOsem )
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOsection,_lhsOxhtml,_lhsOxhtmlTOC,_lhsOxml)))
sem_SectionContents_Nil :: T_SectionContents 
sem_SectionContents_Nil  =
    (\ _lhsIlevel
       _lhsIsection
       _lhsIsem ->
         (let _lhsOlatex :: ([ShowS])
              _lhsOprocessors :: ([Processor Document Syn_Document])
              _lhsOxhtml :: NodeSet
              _lhsOxhtmlTOC :: NodeSet
              _lhsOxml :: ([XL.Content])
              _lhsOsection :: ([Int])
              -- use rule "src/Text/Document/Printer/Latex.ag"(line 75, column 39)
              _lhsOlatex =
                  []
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  []
              -- use rule "src/Text/Document/Printer/Xhtml.ag"(line 58, column 39)
              _lhsOxhtml =
                  []
              -- use rule "src/Text/Document/Plugin/TOC.ag"(line 9, column 52)
              _lhsOxhtmlTOC =
                  []
              -- use rule "src/Text/Document/Printer/Xml.ag"(line 45, column 13)
              _lhsOxml =
                  []
              -- copy rule (chain)
              _lhsOsection =
                  _lhsIsection
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOsection,_lhsOxhtml,_lhsOxhtmlTOC,_lhsOxml)))
-- Structure ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         level                : Int
         sem                  : Document 
      chained attribute:
         section              : [Int]
      synthesized attributes:
         latex                : ShowS
         processors           : [Processor Document Syn_Document]
         xhtml                : Node
         xhtmlTOC             : NodeSet
         xml                  : XL.Content
   alternatives:
      alternative Section:
         child title          : Title 
         child contents       : SectionContents 
-}
data Structure  = Structure_Section (Title) (SectionContents) 
                deriving ( Eq,Show)
-- cata
sem_Structure :: Structure  ->
                 T_Structure 
sem_Structure (Structure_Section _title _contents )  =
    (sem_Structure_Section (sem_Title _title ) (sem_SectionContents _contents ) )
-- semantic domain
type T_Structure  = Int ->
                    ([Int]) ->
                    Document ->
                    ( ShowS,([Processor Document Syn_Document]),([Int]),Node,NodeSet,(XL.Content))
data Inh_Structure  = Inh_Structure {level_Inh_Structure :: Int,section_Inh_Structure :: [Int],sem_Inh_Structure :: Document}
data Syn_Structure  = Syn_Structure {latex_Syn_Structure :: ShowS,processors_Syn_Structure :: [Processor Document Syn_Document],section_Syn_Structure :: [Int],xhtml_Syn_Structure :: Node,xhtmlTOC_Syn_Structure :: NodeSet,xml_Syn_Structure :: XL.Content}
wrap_Structure :: T_Structure  ->
                  Inh_Structure  ->
                  Syn_Structure 
wrap_Structure sem (Inh_Structure _lhsIlevel _lhsIsection _lhsIsem )  =
    (let ( _lhsOlatex,_lhsOprocessors,_lhsOsection,_lhsOxhtml,_lhsOxhtmlTOC,_lhsOxml) =
             (sem _lhsIlevel _lhsIsection _lhsIsem )
     in  (Syn_Structure _lhsOlatex _lhsOprocessors _lhsOsection _lhsOxhtml _lhsOxhtmlTOC _lhsOxml ))
sem_Structure_Section :: T_Title  ->
                         T_SectionContents  ->
                         T_Structure 
sem_Structure_Section title_ contents_  =
    (\ _lhsIlevel
       _lhsIsection
       _lhsIsem ->
         (let _contentsOlevel :: Int
              _contentsOsection :: ([Int])
              _lhsOsection :: ([Int])
              _lhsOxhtml :: Node
              _lhsOxml :: (XL.Content)
              _lhsOlatex :: ShowS
              _lhsOxhtmlTOC :: NodeSet
              _lhsOprocessors :: ([Processor Document Syn_Document])
              _contentsOsem :: Document
              _titleIisTitle :: Bool
              _titleIlatex :: ShowS
              _titleIxhtml :: Node
              _titleIxml :: ([XL.Content])
              _contentsIlatex :: ([ShowS])
              _contentsIprocessors :: ([Processor Document Syn_Document])
              _contentsIsection :: ([Int])
              _contentsIxhtml :: NodeSet
              _contentsIxhtmlTOC :: NodeSet
              _contentsIxml :: ([XL.Content])
              -- "src/Text/Document/Core/Structure.ag"(line 13, column 14)
              _contentsOlevel =
                  _lhsIlevel + 1
              -- "src/Text/Document/Core/Structure.ag"(line 37, column 13)
              _contentsOsection =
                  1 : _lhsIsection
              -- "src/Text/Document/Core/Structure.ag"(line 38, column 13)
              _lhsOsection =
                  let _:x:xs = _contentsIsection in
                  (if _titleIisTitle then (+1) else id)
                  x : xs
              -- "src/Text/Document/Printer/Xhtml.ag"(line 73, column 16)
              _lhsOxhtml =
                  xhtmlSection _lhsIlevel _lhsIsection _titleIisTitle _titleIxhtml _contentsIxhtml
              -- "src/Text/Document/Printer/Xml.ag"(line 71, column 16)
              _lhsOxml =
                  xmlElem "section" (
                    _titleIxml ++
                    _contentsIxml
                  )
              -- "src/Text/Document/Printer/Latex.ag"(line 89, column 16)
              _lhsOlatex =
                  makeTexSection
                  _titleIisTitle
                  _titleIlatex
                  _lhsIlevel
                  _contentsIlatex
              -- "src/Text/Document/Plugin/TOC.ag"(line 15, column 13)
              _lhsOxhtmlTOC =
                  let title = H.emph _titleIxhtml
                      num   = span $ H.text $ intercalate "." $ map show $ reverse $ _lhsIsection
                      item  = nodeset [title, num]
                      f     = if _titleIisTitle then (item :) else id in
                  [nodeset $ f $ xhtmlCleanList _contentsIxhtmlTOC]
              -- use rule "src/Text/Document/Core/Processing.ag"(line 9, column 20)
              _lhsOprocessors =
                  _contentsIprocessors
              -- copy rule (down)
              _contentsOsem =
                  _lhsIsem
              ( _titleIisTitle,_titleIlatex,_titleIxhtml,_titleIxml) =
                  (title_ )
              ( _contentsIlatex,_contentsIprocessors,_contentsIsection,_contentsIxhtml,_contentsIxhtmlTOC,_contentsIxml) =
                  (contents_ _contentsOlevel _contentsOsection _contentsOsem )
          in  ( _lhsOlatex,_lhsOprocessors,_lhsOsection,_lhsOxhtml,_lhsOxhtmlTOC,_lhsOxml)))
-- Title -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         isTitle              : Bool
         latex                : ShowS
         xhtml                : Node
         xml                  : [XL.Content]
   alternatives:
      alternative Just:
         child just           : Inline 
      alternative Nothing:
-}
type Title  = (Maybe (Inline))
-- cata
sem_Title :: Title  ->
             T_Title 
sem_Title (Prelude.Just x )  =
    (sem_Title_Just (sem_Inline x ) )
sem_Title Prelude.Nothing  =
    sem_Title_Nothing
-- semantic domain
type T_Title  = ( Bool,ShowS,Node,([XL.Content]))
data Inh_Title  = Inh_Title {}
data Syn_Title  = Syn_Title {isTitle_Syn_Title :: Bool,latex_Syn_Title :: ShowS,xhtml_Syn_Title :: Node,xml_Syn_Title :: [XL.Content]}
wrap_Title :: T_Title  ->
              Inh_Title  ->
              Syn_Title 
wrap_Title sem (Inh_Title )  =
    (let ( _lhsOisTitle,_lhsOlatex,_lhsOxhtml,_lhsOxml) =
             (sem )
     in  (Syn_Title _lhsOisTitle _lhsOlatex _lhsOxhtml _lhsOxml ))
sem_Title_Just :: T_Inline  ->
                  T_Title 
sem_Title_Just just_  =
    (let _lhsOisTitle :: Bool
         _lhsOxml :: ([XL.Content])
         _lhsOlatex :: ShowS
         _lhsOxhtml :: Node
         _justIlatex :: ShowS
         _justIxhtml :: Node
         _justIxml :: (XL.Content)
         -- "src/Text/Document/Core/Structure.ag"(line 20, column 13)
         _lhsOisTitle =
             True
         -- "src/Text/Document/Printer/Xml.ag"(line 64, column 16)
         _lhsOxml =
             [xmlElem "title" [_justIxml]]
         -- use rule "src/Text/Document/Printer/Latex.ag"(line 73, column 39)
         _lhsOlatex =
             _justIlatex
         -- use rule "src/Text/Document/Printer/Xhtml.ag"(line 56, column 39)
         _lhsOxhtml =
             _justIxhtml
         ( _justIlatex,_justIxhtml,_justIxml) =
             (just_ )
     in  ( _lhsOisTitle,_lhsOlatex,_lhsOxhtml,_lhsOxml))
sem_Title_Nothing :: T_Title 
sem_Title_Nothing  =
    (let _lhsOisTitle :: Bool
         _lhsOxml :: ([XL.Content])
         _lhsOlatex :: ShowS
         _lhsOxhtml :: Node
         -- "src/Text/Document/Core/Structure.ag"(line 21, column 13)
         _lhsOisTitle =
             False
         -- "src/Text/Document/Printer/Xml.ag"(line 63, column 16)
         _lhsOxml =
             []
         -- use rule "src/Text/Document/Printer/Latex.ag"(line 73, column 39)
         _lhsOlatex =
             id
         -- use rule "src/Text/Document/Printer/Xhtml.ag"(line 56, column 39)
         _lhsOxhtml =
             empty
     in  ( _lhsOisTitle,_lhsOlatex,_lhsOxhtml,_lhsOxml))
