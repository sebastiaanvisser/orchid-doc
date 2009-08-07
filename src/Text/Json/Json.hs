

-- UUAGC 0.9.7 (src/Text/Json/Json.ag)
module Text.Json.Json where

import Prelude hiding (null)
import qualified Prelude as P
import Data.List (intercalate)
import Data.Char (toLower)

data Number = R Double | Z Integer


string = Json_String
number = Json_Number
int    = Json_Number . Z
real   = Json_Number . R
object = Json_Object
array  = Json_Array
bool   = Json_Bool
null   = Json_Null
pair   = Pair_Pair
(|:)   = pair


ss = showString

intercalateS c = foldl1 (\a -> ((a.c).))



wrappedSemJson = flip (wrap_Json . sem_Json) Inh_Json

instance Show Json where
  show = jsonToString

jsonToString :: Json -> String
jsonToString = ($"") . pp_Syn_Json . wrappedSemJson

-- Array -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : [ShowS]
   alternatives:
      alternative Cons:
         child hd             : Json 
         child tl             : Array 
      alternative Nil:
-}
type Array  = [(Json)]
-- cata
sem_Array :: Array  ->
             T_Array 
sem_Array list  =
    (Prelude.foldr sem_Array_Cons sem_Array_Nil (Prelude.map sem_Json list) )
-- semantic domain
type T_Array  = ( ([ShowS]))
data Inh_Array  = Inh_Array {}
data Syn_Array  = Syn_Array {pp_Syn_Array :: [ShowS]}
wrap_Array :: T_Array  ->
              Inh_Array  ->
              Syn_Array 
wrap_Array sem (Inh_Array )  =
    (let ( _lhsOpp) =
             (sem )
     in  (Syn_Array _lhsOpp ))
sem_Array_Cons :: T_Json  ->
                  T_Array  ->
                  T_Array 
sem_Array_Cons hd_ tl_  =
    (let _lhsOpp :: ([ShowS])
         _hdIpp :: ShowS
         _tlIpp :: ([ShowS])
         -- "src/Text/Json/Json.ag"(line 63, column 12)
         _lhsOpp =
             _hdIpp : _tlIpp
         ( _hdIpp) =
             (hd_ )
         ( _tlIpp) =
             (tl_ )
     in  ( _lhsOpp))
sem_Array_Nil :: T_Array 
sem_Array_Nil  =
    (let _lhsOpp :: ([ShowS])
         -- "src/Text/Json/Json.ag"(line 62, column 12)
         _lhsOpp =
             []
     in  ( _lhsOpp))
-- Json --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : ShowS
   alternatives:
      alternative Array:
         child a              : Array 
      alternative Bool:
         child b              : {Bool}
      alternative Null:
      alternative Number:
         child n              : {Number}
      alternative Object:
         child p              : Pairs 
      alternative String:
         child s              : {String}
-}
data Json  = Json_Array (Array) 
           | Json_Bool (Bool) 
           | Json_Null 
           | Json_Number (Number) 
           | Json_Object (Pairs) 
           | Json_String (String) 
-- cata
sem_Json :: Json  ->
            T_Json 
sem_Json (Json_Array _a )  =
    (sem_Json_Array (sem_Array _a ) )
sem_Json (Json_Bool _b )  =
    (sem_Json_Bool _b )
sem_Json (Json_Null )  =
    (sem_Json_Null )
sem_Json (Json_Number _n )  =
    (sem_Json_Number _n )
sem_Json (Json_Object _p )  =
    (sem_Json_Object (sem_Pairs _p ) )
sem_Json (Json_String _s )  =
    (sem_Json_String _s )
-- semantic domain
type T_Json  = ( ShowS)
data Inh_Json  = Inh_Json {}
data Syn_Json  = Syn_Json {pp_Syn_Json :: ShowS}
wrap_Json :: T_Json  ->
             Inh_Json  ->
             Syn_Json 
wrap_Json sem (Inh_Json )  =
    (let ( _lhsOpp) =
             (sem )
     in  (Syn_Json _lhsOpp ))
sem_Json_Array :: T_Array  ->
                  T_Json 
sem_Json_Array a_  =
    (let _lhsOpp :: ShowS
         _aIpp :: ([ShowS])
         -- "src/Text/Json/Json.ag"(line 57, column 12)
         _lhsOpp =
             ss "[" . intercalateS (ss ",") _aIpp . ss "]"
         ( _aIpp) =
             (a_ )
     in  ( _lhsOpp))
sem_Json_Bool :: Bool ->
                 T_Json 
sem_Json_Bool b_  =
    (let _lhsOpp :: ShowS
         -- "src/Text/Json/Json.ag"(line 58, column 12)
         _lhsOpp =
             ss $ map toLower $ show b_
     in  ( _lhsOpp))
sem_Json_Null :: T_Json 
sem_Json_Null  =
    (let _lhsOpp :: ShowS
         -- "src/Text/Json/Json.ag"(line 59, column 12)
         _lhsOpp =
             ss "null"
     in  ( _lhsOpp))
sem_Json_Number :: Number ->
                   T_Json 
sem_Json_Number n_  =
    (let _lhsOpp :: ShowS
         -- "src/Text/Json/Json.ag"(line 55, column 12)
         _lhsOpp =
             case n_ of R r -> ss $ show r ; Z z -> ss $ show z
     in  ( _lhsOpp))
sem_Json_Object :: T_Pairs  ->
                   T_Json 
sem_Json_Object p_  =
    (let _lhsOpp :: ShowS
         _pIpp :: ([ShowS])
         -- "src/Text/Json/Json.ag"(line 56, column 12)
         _lhsOpp =
             ss "{" . intercalateS (ss ",") _pIpp . ss "}"
         ( _pIpp) =
             (p_ )
     in  ( _lhsOpp))
sem_Json_String :: String ->
                   T_Json 
sem_Json_String s_  =
    (let _lhsOpp :: ShowS
         -- "src/Text/Json/Json.ag"(line 54, column 12)
         _lhsOpp =
             ss $ show s_
     in  ( _lhsOpp))
-- Pair --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : ShowS
   alternatives:
      alternative Pair:
         child k              : {String}
         child v              : Json 
-}
data Pair  = Pair_Pair (String) (Json) 
-- cata
sem_Pair :: Pair  ->
            T_Pair 
sem_Pair (Pair_Pair _k _v )  =
    (sem_Pair_Pair _k (sem_Json _v ) )
-- semantic domain
type T_Pair  = ( ShowS)
data Inh_Pair  = Inh_Pair {}
data Syn_Pair  = Syn_Pair {pp_Syn_Pair :: ShowS}
wrap_Pair :: T_Pair  ->
             Inh_Pair  ->
             Syn_Pair 
wrap_Pair sem (Inh_Pair )  =
    (let ( _lhsOpp) =
             (sem )
     in  (Syn_Pair _lhsOpp ))
sem_Pair_Pair :: String ->
                 T_Json  ->
                 T_Pair 
sem_Pair_Pair k_ v_  =
    (let _lhsOpp :: ShowS
         _vIpp :: ShowS
         -- "src/Text/Json/Json.ag"(line 66, column 12)
         _lhsOpp =
             ss (show k_) . ss ":" . _vIpp
         ( _vIpp) =
             (v_ )
     in  ( _lhsOpp))
-- Pairs -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : [ShowS]
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
type T_Pairs  = ( ([ShowS]))
data Inh_Pairs  = Inh_Pairs {}
data Syn_Pairs  = Syn_Pairs {pp_Syn_Pairs :: [ShowS]}
wrap_Pairs :: T_Pairs  ->
              Inh_Pairs  ->
              Syn_Pairs 
wrap_Pairs sem (Inh_Pairs )  =
    (let ( _lhsOpp) =
             (sem )
     in  (Syn_Pairs _lhsOpp ))
sem_Pairs_Cons :: T_Pair  ->
                  T_Pairs  ->
                  T_Pairs 
sem_Pairs_Cons hd_ tl_  =
    (let _lhsOpp :: ([ShowS])
         _hdIpp :: ShowS
         _tlIpp :: ([ShowS])
         -- "src/Text/Json/Json.ag"(line 70, column 12)
         _lhsOpp =
             _hdIpp : _tlIpp
         ( _hdIpp) =
             (hd_ )
         ( _tlIpp) =
             (tl_ )
     in  ( _lhsOpp))
sem_Pairs_Nil :: T_Pairs 
sem_Pairs_Nil  =
    (let _lhsOpp :: ([ShowS])
         -- "src/Text/Json/Json.ag"(line 69, column 12)
         _lhsOpp =
             []
     in  ( _lhsOpp))
