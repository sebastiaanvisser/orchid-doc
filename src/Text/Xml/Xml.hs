

-- UUAGC 0.9.14 (src/Text/Xml/Xml.ag)
module Text.Xml.Xml where

import Misc.Commons (ss)

type Namespace = String
type Comment   = String
type Text      = String
type NodePair  = (Node, Node)
type NodeMap   = [NodePair]


doc     = Node_Doc
text    = Node_Text
comment = Node_Comment
nodeset = Node_NodeSet
snippet = Node_Snippet
empty   = Node_Empty

elementNS ns e = Node_Element ns e []
element        = elementNS ""

attributeNS ns k v (Node_Element n' e a s) = Node_Element n' e ((Attribute_Attribute ns k v):a) s
attribute = attributeNS ""

-- Determine whether an XML node has significant data.
significant (Node_NodeSet []) = False
significant (Node_Text    "") = False
significant (Node_Comment "") = False
significant (Node_Empty     ) = False
significant _                 = True



  -- todo: use utils.
ppNodeset []  _     = id
ppNodeset [n] mixed = n
ppNodeset ns  mixed = c . foldl1 (\a -> ((a.c).)) ns . c
  where c = if mixed then id else ss "\n"

ppNs "" = ss ""
ppNs ns = ss (ns ++ ":")

-- Escape special characters in text that will end up in an XML document.
escape :: String -> String
escape []       = []
escape ('<':xs) = "&lt;"   ++ escape xs
escape ('>':xs) = "&gt;"   ++ escape xs
escape ('&':xs) = "&amp;"  ++ escape xs
escape ('"':xs) = "&quot;" ++ escape xs
escape (x:xs)   = x : escape xs



wrappedSemNode = flip (wrap_Node . sem_Node) Inh_Node

instance Show Node where
  show = xmlToString

xmlToString :: Node -> String
xmlToString = ($"") . pp_Syn_Node . wrappedSemNode
-- Attribute ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : ShowS
   alternatives:
      alternative Attribute:
         child namespace      : {Namespace}
         child key            : {String}
         child value          : {String}
-}
data Attribute  = Attribute_Attribute (Namespace) (String) (String) 
                deriving ( Eq)
-- cata
sem_Attribute :: Attribute  ->
                 T_Attribute 
sem_Attribute (Attribute_Attribute _namespace _key _value )  =
    (sem_Attribute_Attribute _namespace _key _value )
-- semantic domain
type T_Attribute  = ( ShowS)
data Inh_Attribute  = Inh_Attribute {}
data Syn_Attribute  = Syn_Attribute {pp_Syn_Attribute :: ShowS}
wrap_Attribute :: T_Attribute  ->
                  Inh_Attribute  ->
                  Syn_Attribute 
wrap_Attribute sem (Inh_Attribute )  =
    (let ( _lhsOpp) =
             (sem )
     in  (Syn_Attribute _lhsOpp ))
sem_Attribute_Attribute :: Namespace ->
                           String ->
                           String ->
                           T_Attribute 
sem_Attribute_Attribute namespace_ key_ value_  =
    (let _lhsOpp :: ShowS
         -- "src/Text/Xml/Xml.ag"(line 105, column 15)
         _lhsOpp =
             ppNs namespace_ . ss key_ . ss "=\"" . ss (escape value_) . ss "\""
     in  ( _lhsOpp))
-- Attributes --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : ShowS
   alternatives:
      alternative Cons:
         child hd             : Attribute 
         child tl             : Attributes 
      alternative Nil:
-}
type Attributes  = [(Attribute)]
-- cata
sem_Attributes :: Attributes  ->
                  T_Attributes 
sem_Attributes list  =
    (Prelude.foldr sem_Attributes_Cons sem_Attributes_Nil (Prelude.map sem_Attribute list) )
-- semantic domain
type T_Attributes  = ( ShowS)
data Inh_Attributes  = Inh_Attributes {}
data Syn_Attributes  = Syn_Attributes {pp_Syn_Attributes :: ShowS}
wrap_Attributes :: T_Attributes  ->
                   Inh_Attributes  ->
                   Syn_Attributes 
wrap_Attributes sem (Inh_Attributes )  =
    (let ( _lhsOpp) =
             (sem )
     in  (Syn_Attributes _lhsOpp ))
sem_Attributes_Cons :: T_Attribute  ->
                       T_Attributes  ->
                       T_Attributes 
sem_Attributes_Cons hd_ tl_  =
    (let _lhsOpp :: ShowS
         _hdIpp :: ShowS
         _tlIpp :: ShowS
         -- "src/Text/Xml/Xml.ag"(line 108, column 15)
         _lhsOpp =
             ss " " . _hdIpp . _tlIpp
         ( _hdIpp) =
             (hd_ )
         ( _tlIpp) =
             (tl_ )
     in  ( _lhsOpp))
sem_Attributes_Nil :: T_Attributes 
sem_Attributes_Nil  =
    (let _lhsOpp :: ShowS
         -- use rule "src/Text/Xml/Xml.ag"(line 93, column 41)
         _lhsOpp =
             id
     in  ( _lhsOpp))
-- Node --------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         mixed                : Bool
         pp                   : ShowS
   alternatives:
      alternative Comment:
         child comment        : {Comment}
      alternative Doc:
         child root           : Node 
      alternative Element:
         child namespace      : {Namespace}
         child name           : {String}
         child attributes     : Attributes 
         child childNodes     : Node 
      alternative Empty:
      alternative NodeSet:
         child nodes          : NodeSet 
      alternative Snippet:
         child snippet        : {String}
      alternative Text:
         child text           : {Text}
-}
data Node  = Node_Comment (Comment) 
           | Node_Doc (Node) 
           | Node_Element (Namespace) (String) (Attributes) (Node) 
           | Node_Empty 
           | Node_NodeSet (NodeSet) 
           | Node_Snippet (String) 
           | Node_Text (Text) 
           deriving ( Eq)
-- cata
sem_Node :: Node  ->
            T_Node 
sem_Node (Node_Comment _comment )  =
    (sem_Node_Comment _comment )
sem_Node (Node_Doc _root )  =
    (sem_Node_Doc (sem_Node _root ) )
sem_Node (Node_Element _namespace _name _attributes _childNodes )  =
    (sem_Node_Element _namespace _name (sem_Attributes _attributes ) (sem_Node _childNodes ) )
sem_Node (Node_Empty )  =
    (sem_Node_Empty )
sem_Node (Node_NodeSet _nodes )  =
    (sem_Node_NodeSet (sem_NodeSet _nodes ) )
sem_Node (Node_Snippet _snippet )  =
    (sem_Node_Snippet _snippet )
sem_Node (Node_Text _text )  =
    (sem_Node_Text _text )
-- semantic domain
type T_Node  = ( Bool,ShowS)
data Inh_Node  = Inh_Node {}
data Syn_Node  = Syn_Node {mixed_Syn_Node :: Bool,pp_Syn_Node :: ShowS}
wrap_Node :: T_Node  ->
             Inh_Node  ->
             Syn_Node 
wrap_Node sem (Inh_Node )  =
    (let ( _lhsOmixed,_lhsOpp) =
             (sem )
     in  (Syn_Node _lhsOmixed _lhsOpp ))
sem_Node_Comment :: Comment ->
                    T_Node 
sem_Node_Comment comment_  =
    (let _lhsOpp :: ShowS
         _lhsOmixed :: Bool
         -- "src/Text/Xml/Xml.ag"(line 102, column 15)
         _lhsOpp =
             ss "<!-- " . ss (escape comment_) . ss " -->"
         -- use rule "src/Text/Xml/Xml.ag"(line 64, column 31)
         _lhsOmixed =
             False
     in  ( _lhsOmixed,_lhsOpp))
sem_Node_Doc :: T_Node  ->
                T_Node 
sem_Node_Doc root_  =
    (let _lhsOmixed :: Bool
         _lhsOpp :: ShowS
         _rootImixed :: Bool
         _rootIpp :: ShowS
         -- use rule "src/Text/Xml/Xml.ag"(line 64, column 31)
         _lhsOmixed =
             _rootImixed
         -- use rule "src/Text/Xml/Xml.ag"(line 93, column 41)
         _lhsOpp =
             _rootIpp
         ( _rootImixed,_rootIpp) =
             (root_ )
     in  ( _lhsOmixed,_lhsOpp))
sem_Node_Element :: Namespace ->
                    String ->
                    T_Attributes  ->
                    T_Node  ->
                    T_Node 
sem_Node_Element namespace_ name_ attributes_ childNodes_  =
    (let _lhsOmixed :: Bool
         _lhsOpp :: ShowS
         _attributesIpp :: ShowS
         _childNodesImixed :: Bool
         _childNodesIpp :: ShowS
         -- "src/Text/Xml/Xml.ag"(line 68, column 13)
         _lhsOmixed =
             False
         -- "src/Text/Xml/Xml.ag"(line 98, column 15)
         _lhsOpp =
             ss "<" . ppNs namespace_ . ss name_ . _attributesIpp . ss ">" .
             _childNodesIpp . ss "</" . ss name_ . ss ">"
         ( _attributesIpp) =
             (attributes_ )
         ( _childNodesImixed,_childNodesIpp) =
             (childNodes_ )
     in  ( _lhsOmixed,_lhsOpp))
sem_Node_Empty :: T_Node 
sem_Node_Empty  =
    (let _lhsOmixed :: Bool
         _lhsOpp :: ShowS
         -- use rule "src/Text/Xml/Xml.ag"(line 64, column 31)
         _lhsOmixed =
             False
         -- use rule "src/Text/Xml/Xml.ag"(line 93, column 41)
         _lhsOpp =
             id
     in  ( _lhsOmixed,_lhsOpp))
sem_Node_NodeSet :: T_NodeSet  ->
                    T_Node 
sem_Node_NodeSet nodes_  =
    (let _lhsOpp :: ShowS
         _lhsOmixed :: Bool
         _nodesImixed :: Bool
         _nodesIpp :: ([ShowS])
         -- "src/Text/Xml/Xml.ag"(line 97, column 15)
         _lhsOpp =
             ppNodeset _nodesIpp _nodesImixed
         -- use rule "src/Text/Xml/Xml.ag"(line 64, column 31)
         _lhsOmixed =
             _nodesImixed
         ( _nodesImixed,_nodesIpp) =
             (nodes_ )
     in  ( _lhsOmixed,_lhsOpp))
sem_Node_Snippet :: String ->
                    T_Node 
sem_Node_Snippet snippet_  =
    (let _lhsOpp :: ShowS
         _lhsOmixed :: Bool
         -- "src/Text/Xml/Xml.ag"(line 101, column 15)
         _lhsOpp =
             ss snippet_
         -- use rule "src/Text/Xml/Xml.ag"(line 64, column 31)
         _lhsOmixed =
             False
     in  ( _lhsOmixed,_lhsOpp))
sem_Node_Text :: Text ->
                 T_Node 
sem_Node_Text text_  =
    (let _lhsOmixed :: Bool
         _lhsOpp :: ShowS
         -- "src/Text/Xml/Xml.ag"(line 67, column 13)
         _lhsOmixed =
             True
         -- "src/Text/Xml/Xml.ag"(line 100, column 15)
         _lhsOpp =
             ss (escape text_)
     in  ( _lhsOmixed,_lhsOpp))
-- NodeSet -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         mixed                : Bool
         pp                   : [ShowS]
   alternatives:
      alternative Cons:
         child hd             : Node 
         child tl             : NodeSet 
      alternative Nil:
-}
type NodeSet  = [(Node)]
-- cata
sem_NodeSet :: NodeSet  ->
               T_NodeSet 
sem_NodeSet list  =
    (Prelude.foldr sem_NodeSet_Cons sem_NodeSet_Nil (Prelude.map sem_Node list) )
-- semantic domain
type T_NodeSet  = ( Bool,([ShowS]))
data Inh_NodeSet  = Inh_NodeSet {}
data Syn_NodeSet  = Syn_NodeSet {mixed_Syn_NodeSet :: Bool,pp_Syn_NodeSet :: [ShowS]}
wrap_NodeSet :: T_NodeSet  ->
                Inh_NodeSet  ->
                Syn_NodeSet 
wrap_NodeSet sem (Inh_NodeSet )  =
    (let ( _lhsOmixed,_lhsOpp) =
             (sem )
     in  (Syn_NodeSet _lhsOmixed _lhsOpp ))
sem_NodeSet_Cons :: T_Node  ->
                    T_NodeSet  ->
                    T_NodeSet 
sem_NodeSet_Cons hd_ tl_  =
    (let _lhsOmixed :: Bool
         _lhsOpp :: ([ShowS])
         _hdImixed :: Bool
         _hdIpp :: ShowS
         _tlImixed :: Bool
         _tlIpp :: ([ShowS])
         -- use rule "src/Text/Xml/Xml.ag"(line 64, column 31)
         _lhsOmixed =
             _hdImixed || _tlImixed
         -- use rule "src/Text/Xml/Xml.ag"(line 94, column 41)
         _lhsOpp =
             _hdIpp : _tlIpp
         ( _hdImixed,_hdIpp) =
             (hd_ )
         ( _tlImixed,_tlIpp) =
             (tl_ )
     in  ( _lhsOmixed,_lhsOpp))
sem_NodeSet_Nil :: T_NodeSet 
sem_NodeSet_Nil  =
    (let _lhsOmixed :: Bool
         _lhsOpp :: ([ShowS])
         -- use rule "src/Text/Xml/Xml.ag"(line 64, column 31)
         _lhsOmixed =
             False
         -- use rule "src/Text/Xml/Xml.ag"(line 94, column 41)
         _lhsOpp =
             []
     in  ( _lhsOmixed,_lhsOpp))
