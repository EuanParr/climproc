{-
an implementation (with some alterations) of the NetCDF CDL grammar defined in
https://manpages.ubuntu.com/manpages/bionic/man1/ncgen.1.html
and https://github.com/Unidata/netcdf-c/blob/main/ncgen/ncgen.l

each nonterminal is represented by a type and a parser producing that type
-}

module CdlParsing where

import qualified Data.List.Split
import qualified Data.Ratio

import CdlLexing
import Parsing

type CdlParser a = Parser CdlToken a
type Ident = String

---- parsers of kinds of token follow

ident :: CdlParser Ident
ident = pop >>= \x -> case x of
                      Identifier y -> pure y
                      _ -> noParse "[CdlParsing.ident] Expected identifier"

stringTok :: CdlParser String
stringTok = pop >>= \x -> case x of
                          StringLiteral y -> pure y
                          _ -> noParse "[CdlParsing.stringtok] Expected string literal"

ratTok :: CdlParser Rational
ratTok = pop >>= \x -> case x of
                        Number y -> pure y
                        _ -> noParse "[CdlParsing.ratTok] Expected number"

intTok :: CdlParser Integer
intTok = ratTok >>= \x -> if Data.Ratio.denominator x == 1 then pure (Data.Ratio.numerator x) else noParse "[CdlParsing] Expected integer"

hexTok :: CdlParser Integer
hexTok = pop >>= \x -> case x of
                        Hex y -> pure y
                        _ -> noParse "[CdlParsing.hexTok] Expected hexadecimal integer"

---- the main grammar follows

data NCDesc = NCDesc DataSetID RootGroup deriving Show

ncDesc :: CdlParser NCDesc
ncDesc = pushCtx "NCDesc" $ do
  _ <- popToken (Identifier "netcdf")
  d <- dataSetID
  r <- rootGroup
  eotP
  pure $ NCDesc d r

newtype DataSetID = DataSetID Ident deriving Show

dataSetID :: CdlParser DataSetID
dataSetID = pushCtx "DataSetID" $ fmap DataSetID ident

data RootGroup = RootGroup GroupBody SubgroupList deriving Show

rootGroup :: CdlParser RootGroup
rootGroup = pushCtx "RootGroup" $ do
  _ <- popToken OpenBracket
  g <- groupBody
  s <- subgroupList
  _ <- popToken CloseBracket
  pure $ RootGroup g s

data GroupBody = GroupBody AttrDeclList TypeSection DimSection VaSection DataSection deriving Show

groupBody :: CdlParser GroupBody
groupBody = pushCtx "GroupBody" $ do
  a <- attrDeclList
  t <- typeSection
  di <- dimSection
  v <- vaSection
  da <- dataSection
  pure $ GroupBody a t di v da

newtype SubgroupList = SubgroupList [NamedGroup] deriving Show

subgroupList :: CdlParser SubgroupList
subgroupList = pushCtx "SubGroupList" $ fmap SubgroupList $ repeatP namedGroup

data NamedGroup = NamedGroup Ident GroupBody SubgroupList AttrDeclList deriving Show
  
namedGroup :: CdlParser NamedGroup
namedGroup = do
  _ <- popToken GROUP
  _ <- popToken Colon
  i <- ident
  _ <- popToken OpenBracket
  g <- groupBody
  s <- subgroupList
  _ <- popToken CloseBracket
  a <- attrDeclList
  pure $ NamedGroup i g s a

data TypeSection = TypeSection0 | TypeSection1 TypeDecls deriving Show

typeSection :: CdlParser TypeSection
typeSection =
  (popToken TYPES >> popToken Colon >> typeDecls >>= \x -> pure (TypeSection1 x))
  `backup`
  (popToken TYPES >> popToken Colon >> pure TypeSection0)
  `backup` pure TypeSection0

newtype TypeDecls = TypeDecls [TypeOrAttrDecl] deriving Show

typeDecls :: CdlParser TypeDecls
typeDecls = fmap TypeDecls $ repeatPos typeOrAttrDecl

newtype TypeName = TypeName Ident deriving Show

typeName :: CdlParser TypeName
typeName = fmap TypeName ident

data TypeOrAttrDecl = TypeOrAttrDecl0 TypeDecl | TypeOrAttrDecl1 AttrDecl deriving Show

typeOrAttrDecl :: CdlParser TypeOrAttrDecl
typeOrAttrDecl = fmap TypeOrAttrDecl0 typeDecl `backup` do
  a <- attrDecl
  _ <- popToken Semicolon
  pure $ TypeOrAttrDecl1 a

data TypeDecl = TypeDecl0 EnumDecl | TypeDecl1 CompoundDecl | TypeDecl2 VLenDecl | TypeDecl3 OpaqueDecl deriving Show

typeDecl :: CdlParser TypeDecl
typeDecl = f TypeDecl0 enumDecl 
           `backup` (fmap TypeDecl1 compoundDecl)
           `backup` (fmap TypeDecl2 vLenDecl)
           `backup` (fmap TypeDecl3 opaqueDecl)
  where f c p = p >>= \x -> (popToken Semicolon `backup` pure Semicolon) >> pure (c x)

data EnumDecl = EnumDecl PrimType TypeName EnumIdList deriving Show

enumDecl :: CdlParser EnumDecl
enumDecl = do
  p <- primType
  _ <- popToken (Identifier "enum")
  t <- typeName
  _ <- popToken OpenBracket
  e <- enumIdList
  _ <- popToken CloseBracket
  pure $ EnumDecl p t e

newtype EnumIdList = EnumIdList [EnumId] deriving Show

enumIdList :: CdlParser EnumIdList
enumIdList = fmap EnumIdList $ repeatPosSep enumId $ popToken Comma

data EnumId = EnumId Ident ConstInt deriving Show

enumId :: CdlParser EnumId
enumId = do
  i <- ident
  _ <- popToken Equals
  c <- constInt
  pure $ EnumId i c

data OpaqueDecl = OpaqueDecl Integer TypeName deriving Show

opaqueDecl :: CdlParser OpaqueDecl
opaqueDecl = do
  _ <- popToken (Identifier "opaque")
  _ <- popToken OpenParen
  r <- intTok
  _ <- popToken CloseParen
  t <- typeName
  pure $ OpaqueDecl r t

data VLenDecl = VLenDecl TypeRef TypeName deriving Show

vLenDecl :: CdlParser VLenDecl
vLenDecl = do
  tr <- typeRef
  _ <- popToken OpenParen
  _ <- popToken Star
  _ <- popToken CloseParen
  tn <- typeName
  pure $ VLenDecl tr tn

data CompoundDecl = CompoundDecl TypeName Fields deriving Show

compoundDecl :: CdlParser CompoundDecl
compoundDecl = do
  _ <- popToken (Identifier "compound") `backup` popToken (Identifier "struct") `backup` popToken (Identifier "structure")
  t <- typeName
  _ <- popToken OpenBracket
  f <- fields
  _ <- popToken CloseBracket
  pure $ CompoundDecl t f

data Fields = Fields [Field] deriving Show

fields :: CdlParser Fields
fields = fmap Fields $ repeatPos $ field <* popToken Semicolon

data Field = Field TypeRef FieldList deriving Show

field :: CdlParser Field
field = do
  t <- typeRef
  f <- fieldList
  pure $ Field t f

data PrimType = CHAR_K | BYTE_K | SHORT_K | INT_K | FLOAT_K | DOUBLE_K | UBYTE_K | USHORT_K | UINT_K | INT64_K | UINT64_K deriving Show

primType :: CdlParser PrimType
primType =
  (popToken (Identifier "char") >> pure CHAR_K)
  `backup`
  (popToken (Identifier "byte") >> pure BYTE_K)
  `backup`
  (popToken (Identifier "short") >> pure SHORT_K)
  `backup`
  (popToken (Identifier "long") >> pure INT_K)
  `backup`
  (popToken (Identifier "integer") >> pure INT_K)
  `backup`
  (popToken (Identifier "float") >> pure FLOAT_K)
  `backup`
  (popToken (Identifier "double") >> pure DOUBLE_K)
  `backup`
  (popToken (Identifier "ubyte") >> pure UBYTE_K)
  `backup`
  (popToken (Identifier "ushort") >> pure USHORT_K)
  `backup`
  (popToken (Identifier "uint") >> pure UINT_K)
  `backup`
  (popToken (Identifier "int64") >> pure INT64_K)
  `backup`
  (popToken (Identifier "uint64") >> pure UINT64_K)

data DimSection = DimSection0 | DimSection1 DimDecls deriving Show

dimSection :: CdlParser DimSection
dimSection = (popToken DIMENSIONS >> (fmap DimSection1 dimDecls `backup` pure DimSection0)) `backup` pure DimSection0

data DimDecls = DimDecls [DimOrAttrDecl] deriving Show

dimDecls :: CdlParser DimDecls
dimDecls = fmap DimDecls $ repeatPos $ dimOrAttrDecl <* popToken Semicolon

data DimOrAttrDecl = DimOrAttrDecl0 DimDeclList | DimOrAttrDecl1 AttrDecl deriving Show

dimOrAttrDecl :: CdlParser DimOrAttrDecl
dimOrAttrDecl = fmap DimOrAttrDecl0 dimDeclList `backup` fmap DimOrAttrDecl1 attrDecl

data DimDeclList = DimDeclList [DimDecl] deriving Show

dimDeclList :: CdlParser DimDeclList
dimDeclList = fmap DimDeclList (repeatPosSep dimDecl (popToken Comma))

data DimDecl = DimDecl0 DimD Integer | DimDecl1 DimD deriving Show

dimDecl :: CdlParser DimDecl
dimDecl = do
  d <- dimD
  _ <- popToken Equals
  f d `backup` g d
  where
    f d = do
      x <- intTok
      pure $ DimDecl0 d x
    g d = (popToken (Identifier "unlimited") `backup` popToken (Identifier "UNLIMITED")) >> pure (DimDecl1 d)

data DimD = DimD Ident deriving Show

dimD :: CdlParser DimD
dimD = fmap DimD ident

data VaSection = VaSection0 | VaSection1 VaDecls deriving Show

vaSection :: CdlParser VaSection
vaSection = (popToken VARIABLES >> (fmap VaSection1 vaDecls `backup` pure VaSection0))

data VaDecls = VaDecls [VaDeclOrAttr] deriving Show

vaDecls :: CdlParser VaDecls
vaDecls = fmap VaDecls $ repeatPos $ vaDeclOrAttr <* popToken Semicolon

data VaDeclOrAttr = VaDeclOrAttr0 VarDecl | VaDeclOrAttr1 AttrDecl deriving Show

vaDeclOrAttr :: CdlParser VaDeclOrAttr
vaDeclOrAttr = (fmap VaDeclOrAttr0 varDecl) `backup` (fmap VaDeclOrAttr1 attrDecl)

data VarDecl = VarDecl TypeRef VarList deriving Show

varDecl :: CdlParser VarDecl
varDecl = do
  t <- typeRef
  v <- varList
  pure $ VarDecl t v

data VarList = VarList [VarSpec] deriving Show

varList :: CdlParser VarList
varList = fmap VarList $ repeatPosSep varSpec $ popToken Comma

data VarSpec = VarSpec Ident DimSpec deriving Show

varSpec :: CdlParser VarSpec
varSpec = do
  i <- ident
  d <- dimSpec
  pure $ VarSpec i d

data DimSpec = DimSpec0 | DimSpec1 DimList deriving Show

dimSpec :: CdlParser DimSpec
dimSpec = (do
  _ <- popToken OpenParen
  d <- dimList
  _ <- popToken CloseParen
  pure $ DimSpec1 d) `backup` pure DimSpec0

data DimList = DimList [DimRef] deriving Show

dimList :: CdlParser DimList
dimList = fmap DimList $ repeatPosSep dimRef $ popToken Comma

data DimRef = DimRef Path deriving Show

dimRef :: CdlParser DimRef
dimRef = fmap DimRef path

data FieldList = FieldList [FieldSpec] deriving Show

fieldList :: CdlParser FieldList
fieldList = fmap FieldList $ repeatPosSep fieldSpec $ popToken Comma

data FieldSpec = FieldSpec Ident FieldDimSpec deriving Show

fieldSpec :: CdlParser FieldSpec
fieldSpec = do
  i <- ident
  f <- fieldDimSpec
  pure $ FieldSpec i f

data FieldDimSpec = FieldDimSpec0 | FieldDimSpec1 FieldDimList deriving Show

fieldDimSpec :: CdlParser FieldDimSpec
fieldDimSpec = (do
  _ <- popToken OpenParen
  f <- fieldDimList
  _ <- popToken CloseParen
  pure $ FieldDimSpec1 f) `backup` pure FieldDimSpec0

data FieldDimList = FieldDimList [FieldDim] deriving Show

fieldDimList :: CdlParser FieldDimList
fieldDimList = fmap FieldDimList $ repeatPosSep fieldDim $ popToken Comma

data FieldDim = FieldDim Integer deriving Show

fieldDim :: CdlParser FieldDim
fieldDim = fmap FieldDim intTok

data VarRef = VarRef TypeVarRef deriving Show

varRef :: CdlParser VarRef
varRef = fmap VarRef typeVarRef

data TypeRef = TypeRef TypeVarRef deriving Show

typeRef :: CdlParser TypeRef
typeRef = fmap TypeRef typeVarRef

data TypeVarRef = TypeVarRef0 Path | TypeVarRef1 PrimType deriving Show

typeVarRef :: CdlParser TypeVarRef
typeVarRef = fmap TypeVarRef0 path `backup` fmap TypeVarRef1 primType

data AttrDeclList = AttrDeclList [AttrDecl] deriving Show

attrDeclList :: CdlParser AttrDeclList
attrDeclList = fmap AttrDeclList $ repeatSep attrDecl $ popToken Semicolon

data AttrDecl = AttrDecl0 Ident DataList
              | AttrDecl1 TypeRef TypeVarRef Ident DataList
              | AttrDecl2 TypeVarRef Ident DataList
              | AttrDecl3 TypeVarRef DataList -- _FillValue
              | AttrDecl4 TypeRef TypeVarRef DataList -- _FillValue
              | AttrDecl5 TypeVarRef ConstString -- _Storage
              | AttrDecl6 TypeVarRef IntList -- _ChunkSizes
              | AttrDecl7 TypeVarRef ConstBool -- _Fletcher32
              | AttrDecl8 TypeVarRef ConstInt -- _DeflateLevel
              | AttrDecl9 TypeVarRef ConstBool -- _Shuffle
              | AttrDecl10 TypeVarRef ConstString -- _Endianness
              | AttrDecl11 TypeVarRef ConstBool -- _NoFill
              | AttrDecl12 ConstString -- _Format
               deriving Show

attrDecl :: CdlParser AttrDecl
attrDecl =
  (popToken Colon >> popToken (Identifier "_Format") >> popToken Equals >> fmap AttrDecl12 constString)
  `backup`
  (typeVarRef >>= \t -> popToken Colon >> popToken (Identifier "_NoFill") >> popToken Equals >> constBool >>= \c -> pure (AttrDecl11 t c))
  `backup`
  (typeVarRef >>= \t -> popToken Colon >> popToken (Identifier "_Endianness") >> popToken Equals >> constString >>= \c -> pure (AttrDecl10 t c))
  `backup`
  (typeVarRef >>= \t -> popToken Colon >> popToken (Identifier "_Shuffle") >> popToken Equals >> constBool >>= \c -> pure (AttrDecl9 t c))
  `backup`
  (typeVarRef >>= \t -> popToken Colon >> popToken (Identifier "_DeflateLevel") >> popToken Equals >> constInt >>= \c -> pure (AttrDecl8 t c))
  `backup`
  (typeVarRef >>= \t -> popToken Colon >> popToken (Identifier "_Fletcher32") >> popToken Equals >> constBool >>= \c -> pure (AttrDecl7 t c))
  `backup`
  (typeVarRef >>= \t -> popToken Colon >> popToken (Identifier "_ChunkSizes") >> popToken Equals >> intList >>= \c -> pure (AttrDecl6 t c))
  `backup`
  (typeVarRef >>= \t -> popToken Colon >> popToken (Identifier "_Storage") >> popToken Equals >> constString >>= \c -> pure (AttrDecl5 t c))
  `backup`
  (typeRef >>= \r -> typeVarRef >>= \t -> popToken Colon >> popToken (Identifier "_FillValue") >> popToken Equals >> dataList >>= \l -> pure (AttrDecl4 r t l))
  `backup`
  (typeVarRef >>= \t -> popToken Colon >> popToken (Identifier "_FillValue") >> popToken Equals >> dataList >>= \l -> pure (AttrDecl3 t l))
  `backup`
  (typeVarRef >>= \t -> popToken Colon >> ident >>= \i -> popToken Equals >> dataList >>= \l -> pure (AttrDecl2 t i l))
  `backup`
  (typeRef >>= \r -> typeVarRef >>= \t -> popToken Colon >> ident >>= \i -> popToken Equals >> dataList >>= \l -> pure (AttrDecl1 r t i l))
  `backup`
  (popToken Colon >> ident >>= \i -> popToken Equals >> dataList >>= \l -> pure (AttrDecl0 i l))

data Path = Path [Ident]  deriving Show

path :: CdlParser Path
path = fmap (Path . Data.List.Split.splitOn "/") ident

data DataSection = DataSection0 | DataSection1 DataDecls deriving Show

dataSection :: CdlParser DataSection
dataSection = (popToken DATA >> (fmap DataSection1 dataDecls `backup` pure DataSection0)) `backup` pure DataSection0

data DataDecls = DataDecls [DataDecl] deriving Show

dataDecls :: CdlParser DataDecls
dataDecls = fmap DataDecls $ repeatPos $ (dataDecl <* popToken Semicolon)

data DataDecl = DataDecl VarRef DataList deriving Show

dataDecl :: CdlParser DataDecl
dataDecl = do
  v <- varRef
  _ <- popToken Equals
  d <- dataList
  pure $ DataDecl v d

data DataList = DataList [DataItem] deriving Show

dataList :: CdlParser DataList
dataList = fmap DataList $ repeatSep dataItem $ popToken Comma

data DataItem = DataItem0 ConstData | DataItem1 DataList deriving Show

dataItem :: CdlParser DataItem
dataItem = (popToken OpenBracket >> dataList >>= \x -> popToken CloseBracket >> pure (DataItem1 x))
           `backup` fmap DataItem0 constData

data ConstData = ConstData0 SimpleConstant
               | ConstData1 Integer -- OPAQUESTRING
               | ConstData2 -- FILLMARKER
               | ConstData3 -- NIL
               | ConstData4 EConstRef
               | ConstData5 Function
               deriving Show

constData :: CdlParser ConstData
constData =
  (fmap ConstData0 simpleConstant)
  `backup`
  (fmap ConstData1 hexTok)
  `backup`
  (popToken (Identifier "_") >> pure ConstData2)
  `backup`
  (popToken (Identifier "nil") >> pure ConstData3)
  `backup`
  (popToken (Identifier "Nil") >> pure ConstData3)
  `backup`
  (popToken (Identifier "NIL") >> pure ConstData3)
  `backup`
  fmap ConstData5 function
  `backup`
  fmap ConstData4 eConstRef

data EConstRef = EConstRef Path deriving Show

eConstRef :: CdlParser EConstRef
eConstRef = fmap EConstRef path

data Function = Function Ident ArgList deriving Show

function :: CdlParser Function
function = do
  i <- ident
  _ <- popToken OpenParen
  a <- argList
  _ <- popToken CloseParen
  pure $ Function i a

data ArgList = ArgList [SimpleConstant] deriving Show

argList :: CdlParser ArgList
argList = fmap ArgList $ repeatSep simpleConstant $ popToken Comma

data SimpleConstant = IntConst ConstInt | RatConst ConstRat | StringConst ConstString deriving Show

simpleConstant :: CdlParser SimpleConstant
simpleConstant =
  (fmap IntConst constInt)
  `backup`
  (fmap RatConst constRat)
  `backup`
  (fmap StringConst constString)

data IntList = IntList [ConstInt] deriving Show

intList :: CdlParser IntList
intList = fmap IntList $ repeatSep constInt $ popToken Comma

data ConstInt = ConstInt Integer deriving Show

constInt :: CdlParser ConstInt
constInt = fmap ConstInt intTok

data ConstRat = ConstRat Rational deriving Show

constRat :: CdlParser ConstRat
constRat = fmap ConstRat ratTok

data ConstString = ConstString String deriving Show

constString :: CdlParser ConstString
constString = fmap ConstString stringTok

data ConstBool = ConstBool0 ConstString | ConstBool1 ConstInt deriving Show

constBool :: CdlParser ConstBool
constBool = fmap ConstBool0 constString `backup` fmap ConstBool1 constInt
