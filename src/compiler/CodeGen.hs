{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CodeGen
  ( ppCgen
  ) where

import Language.P4

import Language.C.Quote.C
import qualified Language.C.Syntax as C
import Text.PrettyPrint.Mainland


ppCgen :: Program -> String
ppCgen (Program xs) = pretty 80 $ ppr $ prepend $ cgen xs

prepend :: [C.Definition] -> [C.Definition]
prepend defs = [cunit|
                $esc:("#include \"src/rts/rts.h\"")
                int main() {
                    return 0;
                }
               |] ++ defs

cgen :: [Decl] -> [C.Definition]
cgen xs = (concatMap headers xs) ++ (pktInst xs)


pktInst :: [Decl] -> [C.Definition]
pktInst decls =
  [cunit|struct packet_instance_t {
           typename uint8_t *bytes;
           int len;
           int current_offset;
           $sdecls:(concatMap cInstances decls)
         };
  |]

cInstances :: Decl -> [C.FieldGroup]
cInstances (ScalarInstanceDecl hdrTy name) = [
    [csdecl|struct $id:hdrTy $id:name;|]
  , [csdecl|int $id:(name ++ "_valid");|]
  ]
cInstances (ArrayInstanceDecl hdrTy name cnt) = [
    [csdecl|struct $id:hdrTy $id:name[$int:(c2i cnt)];|]
  , [csdecl|int $id:(name ++ "_valid")[$int:(c2i cnt)];|]
  , [csdecl|int $id:(name ++ "_idx");|]
  ]
cInstances _ = []

headers :: Decl -> [C.Definition]
headers (HeaderTypeDecl name body) = cHeaderTypeDecl name body
headers _ = []

cHeaderTypeDecl :: String -> HeaderDecBody -> [C.Definition]
cHeaderTypeDecl name (HeaderDecBody fields _ _) =
  [cunit|struct $id:name {
             typename uint8_t *bytes;
             $sdecls:(map cFieldDec fields)
         };
         void $id:(name ++ "_to_xp")(struct $id:name *$id:name) {
             $stms:(map (cFieldDecXp "to" name) (calcFields fields))
         }
         void $id:(name ++ "_from_xp")(struct $id:name *$id:name) {
             $stms:(map (cFieldDecXp "from" name) (calcFields fields))
         }
  |]

calcFields :: [FieldDec] -> [(FieldName, Integer, Integer)]
calcFields xs = zip3 names offsets widths
  where
    names   = map (\(FieldDec n _ _) -> n) xs
    offsets = init $ scanl (+) 0 widths
    widths  = map (\(FieldDec _ (BwValue val) _) -> c2i val) xs

cFieldDec :: FieldDec -> C.FieldGroup
cFieldDec (FieldDec name (BwValue val) _) =
  [csdecl|typename uint8_t $id:name[$int:len];|]
  where
    len = ((c2i val) `quot` 8) + if ((c2i val) `mod` 8) == 0 then 0 else 1
cFieldDec x = error $ "Codegen FieldDec: " ++ show x

cFieldDecXp :: String -> String -> (FieldName, Integer, Integer) -> C.Stm
cFieldDecXp toOrFrom hName (name, offset, width) =
  [cstm|$id:(toOrFrom ++ "_xp")($id:hName->bytes, $id:hName->$id:name, $int:offset, $int:width);|]
