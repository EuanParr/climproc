{-
conversion of the AST produced by the CdlParsing grammar into a structure useful for data lookup
-}

module CdlProcessing where

import qualified Data.Char
import qualified Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import qualified CdlLexing
import qualified CdlParsing as P
import qualified Parsing

---- the useful structure follows

-- a group may have multiple attributes with the same subject, but types, dimensions, variable descriptions, variable data assignments, and subgroups all have unique names within their kind
data Group = Group String (Map String [AttrDecl]) (Map String TypeDecl) (Map String DimDecl) (Map String VarDecl) (Map String DataDecl) (Map String Group) deriving Show

data TypeDecl = EnumDecl PrimType String [EnumId] | CompoundDecl String [Field] | VLenDecl String String | OpaqueDecl Integer String deriving Show

data EnumId = EnumId String Integer deriving Show

data Field = Field String [FieldSpec] deriving Show

data PrimType = CHAR_K | BYTE_K | SHORT_K | INT_K | FLOAT_K | DOUBLE_K | UBYTE_K | USHORT_K | UINT_K | INT64_K | UINT64_K deriving Show

data DimDecl = DimDeclLimited String Integer | DimDeclUnlimited String deriving Show

data VarDecl = VarDecl String String [String] deriving Show

data FieldSpec = FieldSpec String [Integer] deriving Show

data AttrDecl = AttributeGlobal String [DataItem]
              | AttributeVariable String String [DataItem]
              | AttributeVariableTyped String String String [DataItem]
              | AttributeFillValue String [DataItem]
              | AttributeFillValueTyped String String [DataItem]
              | AttributeStorage String String
              | AttributeChunkSizes String [Integer]
              | AttributeFletcher32 String Bool
              | AttributeDeflateLevel String Integer
              | AttributeShuffle String Bool
              | AttributeEndianness String String
              | AttributeNoFill String Bool
              | AttributeFormat String
              deriving Show

data DataDecl = DataDecl String [DataItem] deriving Show

data DataItem = Datum Datum | DataItem [DataItem] deriving Show

data Datum = SimpleDatum SimpleConstant
               | OpaqueStringDatum Integer
               | FillMarkerDatum
               | NilDatum
               | EnumDatum String
               | FunctionDatum Function
               deriving Show

data Function = Function String [SimpleConstant] deriving Show

data SimpleConstant = IntConstant Integer | RatConstant Rational | StringConstant String deriving Show

---- intermediate forms not used in the final structure follow

data GroupBody = GroupBody (Map String [AttrDecl]) (Map String TypeDecl) (Map String DimDecl) (Map String VarDecl) (Map String DataDecl) deriving Show

data TypeOrAttrDecl = TypeDecl TypeDecl | TypeAttrDecl AttrDecl deriving Show

data DimOrAttrDecl = DimDecl DimDecl | DimAttrDecl AttrDecl deriving Show

data VaDeclOrAttr = VaDecl VarDecl | VarAttrDecl AttrDecl deriving Show

data VarSpec = VarSpec String [String] deriving Show

---- processing utility functions follow

-- the thing that an attribute is about - [] if it is the enclosing group
attributeSubject :: AttrDecl -> String
attributeSubject (AttributeGlobal _ _) = []
attributeSubject (AttributeVariable n _ _) = n
attributeSubject (AttributeVariableTyped _ n _ _) = n
attributeSubject (AttributeFillValue n _) = n
attributeSubject (AttributeFillValueTyped _ n _) = n
attributeSubject (AttributeStorage n _) = n
attributeSubject (AttributeChunkSizes n _) = n
attributeSubject (AttributeFletcher32 n _) = n
attributeSubject (AttributeDeflateLevel n _) = n
attributeSubject (AttributeShuffle n _) = n
attributeSubject (AttributeEndianness n _) = n
attributeSubject (AttributeNoFill n _) = n
attributeSubject (AttributeFormat _) = []

addAttribute :: AttrDecl -> Map String [AttrDecl] -> Map String [AttrDecl]
addAttribute a m = Map.alter f n m
  where
    n = attributeSubject a
    f Nothing = Just [a]
    f (Just as) = Just (a:as)

addAttributes :: Map String [AttrDecl] -> [AttrDecl] -> Map String [AttrDecl]
addAttributes = foldr addAttribute

typeName :: TypeDecl -> String
typeName (EnumDecl _ n _) = n
typeName (CompoundDecl n _) = n
typeName (VLenDecl _ n) = n
typeName (OpaqueDecl _ n) = n

dimName :: DimDecl -> String
dimName (DimDeclLimited n _) = n
dimName (DimDeclUnlimited n) = n

varName :: VarDecl -> String
varName (VarDecl _ n _) = n

datName :: DataDecl -> String
datName (DataDecl n _) = n

groupName :: Group -> String
groupName (Group n _ _ _ _ _ _) = n

-- convert a list to a map, given a way to get the names/keys of its elements
mapify :: (a -> String) -> [a] -> Map String a
mapify namer xs = Map.fromList $ map (\x -> (namer x, x)) xs

-- add attributes to a group, useful because CDL allows attributes to be specified in various places
addTrailingAttrDecls :: Group -> [AttrDecl] -> Group
addTrailingAttrDecls (Group name attrs types dims vars datas groups) attrDecls = (Group name attrs' types dims vars datas groups)
  where
    attrs' = addAttributes attrs attrDecls

parse :: String -> Group
parse = refineNCDesc . Parsing.parse P.ncDesc . Parsing.parse CdlLexing.token

refine :: P.NCDesc -> Group
refine = refineNCDesc

---- AST processing functions follow, named after the relevant AST part

refineNCDesc :: P.NCDesc -> Group
refineNCDesc (P.NCDesc i r) = addTrailingAttrDecls (Group name attrs types dims vars datas subgroups) $ concatMap snd subgroupsAndAttrs
  where
    name = refineDataSetID i
    (GroupBody attrs types dims vars datas, subgroupsAndAttrs) = refineRootGroup r
    subgroups = mapify groupName $ map fst subgroupsAndAttrs

refineDataSetID :: P.DataSetID -> String
refineDataSetID (P.DataSetID i) = i

refineRootGroup :: P.RootGroup -> (GroupBody, [(Group, [AttrDecl])])
refineRootGroup (P.RootGroup g l) = (refineGroupBody g, refineSubgroupList l) 

refineGroupBody :: P.GroupBody -> GroupBody
refineGroupBody (P.GroupBody a t di v da) = GroupBody attrs' types' dims' vars' dats'
  where
    attrs0 = (refineAttrDeclList a)
    (types, attrs1) = (refineTypeSection t)
    (dims, attrs2) = (refineDimSection di)
    (vars, attrs3) = (refineVaSection v)
    dats = (refineDataSection da)
    attrs' = Map.empty `addAttributes` attrs0 `addAttributes` attrs1 `addAttributes` attrs2 `addAttributes` attrs3
    types' = mapify typeName types
    dims' = mapify dimName dims
    vars' = mapify varName vars
    dats' = mapify datName dats

refineSubgroupList :: P.SubgroupList -> [(Group, [AttrDecl])]
refineSubgroupList (P.SubgroupList ns) = map refineNamedGroup ns

refineNamedGroup :: P.NamedGroup -> (Group, [AttrDecl])
refineNamedGroup (P.NamedGroup name g s a) = (Group name attrsFull types dims vars datas subgroups, followingAttrDecls)
  where
    (GroupBody attrs types dims vars datas) = refineGroupBody g
    subgroupsAndAttrs = refineSubgroupList s
    subgroups = mapify groupName $ map fst subgroupsAndAttrs
    attrsFull = addAttributes attrs $ concatMap snd subgroupsAndAttrs
    followingAttrDecls = refineAttrDeclList a

refineTypeSection :: P.TypeSection -> ([TypeDecl], [AttrDecl])
refineTypeSection P.TypeSection0 = ([], [])
refineTypeSection (P.TypeSection1 t) = foldr f ([], []) $ refineTypeDecls t
  where
    f (TypeDecl t') (ts, as) = (t':ts, as)
    f (TypeAttrDecl a) (ts, as) = (ts, a:as)

refineTypeDecls :: P.TypeDecls -> [TypeOrAttrDecl]
refineTypeDecls (P.TypeDecls ts) = map refineTypeOrAttrDecl ts

refineTypeName :: P.TypeName -> String
refineTypeName (P.TypeName i) = i

refineTypeOrAttrDecl :: P.TypeOrAttrDecl -> TypeOrAttrDecl
refineTypeOrAttrDecl (P.TypeOrAttrDecl0 t) = TypeDecl (refineTypeDecl t)
refineTypeOrAttrDecl (P.TypeOrAttrDecl1 a) = TypeAttrDecl (refineAttrDecl a)

refineTypeDecl :: P.TypeDecl -> TypeDecl
refineTypeDecl (P.TypeDecl0 (P.EnumDecl p t e)) = EnumDecl (refinePrimType p) (refineTypeName t) (refineEnumIdList e)
refineTypeDecl (P.TypeDecl1 (P.CompoundDecl t f)) = CompoundDecl (refineTypeName t) (refineFields f)
refineTypeDecl (P.TypeDecl2 (P.VLenDecl tr tn)) = VLenDecl (refineTypeRef tr) (refineTypeName tn)
refineTypeDecl (P.TypeDecl3 (P.OpaqueDecl i t)) = OpaqueDecl (i) (refineTypeName t)

refineEnumIdList :: P.EnumIdList -> [EnumId]
refineEnumIdList (P.EnumIdList es) = map refineEnumId es

refineEnumId :: P.EnumId -> EnumId
refineEnumId (P.EnumId s c) = EnumId (s) (refineConstInt c)

refineFields :: P.Fields -> [Field]
refineFields (P.Fields fs) = map refineField fs

refineField :: P.Field -> Field
refineField (P.Field t f) = Field (refineTypeRef t) (refineFieldList f)

refinePrimType :: P.PrimType -> PrimType
refinePrimType P.CHAR_K = CHAR_K
refinePrimType P.BYTE_K = BYTE_K
refinePrimType P.SHORT_K = SHORT_K
refinePrimType P.INT_K = INT_K
refinePrimType P.FLOAT_K = FLOAT_K
refinePrimType P.DOUBLE_K = DOUBLE_K
refinePrimType P.UBYTE_K = UBYTE_K
refinePrimType P.USHORT_K = USHORT_K
refinePrimType P.UINT_K = UINT_K
refinePrimType P.INT64_K = INT64_K
refinePrimType P.UINT64_K = UINT64_K

refineDimSection :: P.DimSection -> ([DimDecl], [AttrDecl])
refineDimSection P.DimSection0 = ([], [])
refineDimSection (P.DimSection1 d) = foldr f ([], []) $ refineDimDecls d
  where
    f (DimDecl d') (ds, as) = (d':ds, as)
    f (DimAttrDecl a) (ds, as) = (ds, a:as)

refineDimDecls :: P.DimDecls -> [DimOrAttrDecl]
refineDimDecls (P.DimDecls ds) = concatMap refineDimOrAttrDecl ds

refineDimOrAttrDecl :: P.DimOrAttrDecl -> [DimOrAttrDecl]
refineDimOrAttrDecl (P.DimOrAttrDecl0 d) = map DimDecl (refineDimDeclList d)
refineDimOrAttrDecl (P.DimOrAttrDecl1 a) = [DimAttrDecl (refineAttrDecl a)]

refineDimDeclList :: P.DimDeclList -> [DimDecl]
refineDimDeclList (P.DimDeclList ds) = map refineDimDecl ds

refineDimDecl :: P.DimDecl -> DimDecl
refineDimDecl (P.DimDecl0 d i) = DimDeclLimited (refineDimD d) i
refineDimDecl (P.DimDecl1 d) = DimDeclUnlimited (refineDimD d)

refineDimD :: P.DimD -> String
refineDimD (P.DimD i) = i

refineVaSection :: P.VaSection -> ([VarDecl], [AttrDecl])
refineVaSection P.VaSection0 = ([], [])
refineVaSection (P.VaSection1 v) = foldr f ([], []) $ refineVaDecls v
  where
    f (VaDecl v') (vs, as) = (v':vs, as)
    f (VarAttrDecl a) (vs, as) = (vs, a:as)

refineVaDecls :: P.VaDecls -> [VaDeclOrAttr]
refineVaDecls (P.VaDecls vs) = concatMap refineVaDeclOrAttr vs

refineVaDeclOrAttr :: P.VaDeclOrAttr -> [VaDeclOrAttr]
refineVaDeclOrAttr (P.VaDeclOrAttr0 (P.VarDecl t v)) = let t' = (refineTypeRef t) in map (\(VarSpec n ds) -> VaDecl (VarDecl t' n ds)) (refineVarList v)
refineVaDeclOrAttr (P.VaDeclOrAttr1 a) = [VarAttrDecl (refineAttrDecl a)]

refineVarList :: P.VarList -> [VarSpec]
refineVarList (P.VarList vs) = map refineVarSpec vs

refineVarSpec :: P.VarSpec -> VarSpec
refineVarSpec (P.VarSpec i d) = VarSpec (i) (refineDimSpec d)

refineDimSpec :: P.DimSpec -> [String]
refineDimSpec P.DimSpec0 = []
refineDimSpec (P.DimSpec1 d) = refineDimList d

refineDimList :: P.DimList -> [String]
refineDimList (P.DimList ds) = map refineDimRef ds

refineDimRef :: P.DimRef -> String
refineDimRef (P.DimRef p) = refinePath p

refineFieldList :: P.FieldList -> [FieldSpec]
refineFieldList (P.FieldList fs) = map refineFieldSpec fs

refineFieldSpec :: P.FieldSpec -> FieldSpec
refineFieldSpec (P.FieldSpec i f) = FieldSpec (i) (refineFieldDimSpec f)

refineFieldDimSpec :: P.FieldDimSpec -> [Integer]
refineFieldDimSpec P.FieldDimSpec0 = []
refineFieldDimSpec (P.FieldDimSpec1 f) = refineFieldDimList f

refineFieldDimList :: P.FieldDimList -> [Integer]
refineFieldDimList (P.FieldDimList fs) = map refineFieldDim fs

refineFieldDim :: P.FieldDim -> Integer
refineFieldDim (P.FieldDim i) = i

refineVarRef :: P.VarRef -> String
refineVarRef (P.VarRef t) = refineTypeVarRef t

refineTypeRef :: P.TypeRef -> String
refineTypeRef (P.TypeRef t) = refineTypeVarRef t

refineTypeVarRef :: P.TypeVarRef -> String
refineTypeVarRef (P.TypeVarRef0 p) = refinePath p
refineTypeVarRef (P.TypeVarRef1 p) = error $ "used as a type or variable reference: " ++ show p

refineAttrDeclList :: P.AttrDeclList -> [AttrDecl]
refineAttrDeclList (P.AttrDeclList as) = map refineAttrDecl as

refineAttrDecl :: P.AttrDecl -> AttrDecl
refineAttrDecl (P.AttrDecl0 i d) = AttributeGlobal (i) (refineDataList d)
refineAttrDecl (P.AttrDecl1 t t' i d) = AttributeVariableTyped (refineTypeRef t) (refineTypeVarRef t') (i) (refineDataList d)
refineAttrDecl (P.AttrDecl2 t i d) = AttributeVariable (refineTypeVarRef t) (i) (refineDataList d)
refineAttrDecl (P.AttrDecl3 t d) = AttributeFillValue (refineTypeVarRef t) (refineDataList d)
refineAttrDecl (P.AttrDecl4 t t' d) = AttributeFillValueTyped (refineTypeRef t) (refineTypeVarRef t') (refineDataList d)
refineAttrDecl (P.AttrDecl5 t c) = AttributeStorage (refineTypeVarRef t) (refineConstString c)
refineAttrDecl (P.AttrDecl6 t i) = AttributeChunkSizes (refineTypeVarRef t) (refineIntList i)
refineAttrDecl (P.AttrDecl7 t c) = AttributeFletcher32 (refineTypeVarRef t) (refineConstBool c)
refineAttrDecl (P.AttrDecl8 t c) = AttributeDeflateLevel (refineTypeVarRef t) (refineConstInt c)
refineAttrDecl (P.AttrDecl9 t c) = AttributeShuffle (refineTypeVarRef t) (refineConstBool c)
refineAttrDecl (P.AttrDecl10 t c) = AttributeEndianness (refineTypeVarRef t) (refineConstString c)
refineAttrDecl (P.AttrDecl11 t c) = AttributeNoFill (refineTypeVarRef t) (refineConstBool c)
refineAttrDecl (P.AttrDecl12 c) = AttributeFormat (refineConstString c)

refinePath :: P.Path -> String
refinePath (P.Path is) = Data.List.intercalate "/" is

refineDataSection :: P.DataSection -> [DataDecl]
refineDataSection P.DataSection0 = []
refineDataSection (P.DataSection1 d) = refineDataDecls d

refineDataDecls :: P.DataDecls -> [DataDecl]
refineDataDecls (P.DataDecls ds) = map refineDataDecl ds

refineDataDecl :: P.DataDecl -> DataDecl
refineDataDecl (P.DataDecl v d) = DataDecl (refineVarRef v) (refineDataList d)

refineDataList :: P.DataList -> [DataItem]
refineDataList (P.DataList ds) = map refineDataItem ds

refineDataItem :: P.DataItem -> DataItem
refineDataItem (P.DataItem0 c) = Datum (refineConstData c)
refineDataItem (P.DataItem1 d) = DataItem (refineDataList d)

refineConstData :: P.ConstData -> Datum
refineConstData (P.ConstData0 s) = SimpleDatum (refineSimpleConstant s)
refineConstData (P.ConstData1 i) = OpaqueStringDatum i
refineConstData P.ConstData2 = FillMarkerDatum
refineConstData P.ConstData3 = NilDatum
refineConstData (P.ConstData4 e) = EnumDatum (refineEConstRef e)
refineConstData (P.ConstData5 f) = FunctionDatum (refineFunction f)

refineEConstRef :: P.EConstRef -> String
refineEConstRef (P.EConstRef p) = refinePath p

refineFunction :: P.Function -> Function
refineFunction (P.Function i a) = Function (i) (refineArgList a)

refineArgList :: P.ArgList -> [SimpleConstant]
refineArgList (P.ArgList ss) = map refineSimpleConstant ss 

refineSimpleConstant :: P.SimpleConstant -> SimpleConstant
refineSimpleConstant (P.IntConst c) = IntConstant (refineConstInt c)
refineSimpleConstant (P.RatConst c) = RatConstant (refineConstRat c)
refineSimpleConstant (P.StringConst c) = StringConstant (refineConstString c)

refineIntList :: P.IntList -> [Integer]
refineIntList (P.IntList cs) = map refineConstInt cs

refineConstInt :: P.ConstInt -> Integer
refineConstInt (P.ConstInt i) = i

refineConstRat :: P.ConstRat -> Rational
refineConstRat (P.ConstRat r) = r

refineConstString :: P.ConstString -> String
refineConstString (P.ConstString s) = s

refineConstBool :: P.ConstBool -> Bool
refineConstBool (P.ConstBool0 s) = case map Data.Char.toLower (refineConstString s) of
                                     "false" -> False
                                     "true" -> True
                                     "0" -> False
                                     "1" -> True
                                     s' -> error $ "Invalid boolean: " ++ s'
refineConstBool (P.ConstBool1 i) = case refineConstInt i of
                                     0 -> False
                                     1 -> True
                                     i' -> error $ "Invalid boolean: " ++ show i'
