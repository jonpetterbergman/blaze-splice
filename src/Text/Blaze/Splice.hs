{-# language TemplateHaskell, TupleSections #-}
module Text.Blaze.Splice where

import Text.XML.Light.Input(parseXML)
import Text.XML.Light.Types(Element(..),
                            QName(..),
                            CData(..),
                            Content(..),
                            CDataKind(..),
                            Attr(..))
import qualified Data.Text.IO as TIO
import Language.Haskell.TH(runIO,Q,Exp(..),Lit(..),mkName)
import Text.Blaze.Internal(customLeaf,stringTag,customParent,customAttribute)
import Text.Blaze.Html(toHtml,(!),toValue,preEscapedToHtml)
import Data.Maybe(catMaybes)
import Data.Char(isSpace,isAlpha)
import Language.Haskell.TH.Quote(QuasiQuoter(..),quoteFile)

tok :: String -> [Either String String]
tok x = tokStr x "" []

tokVar :: String -> String -> [Either String String] -> [Either String String]
tokVar [] varAcc acc = reverse $ (Left $ reverse varAcc):acc
tokVar (x:xs) varAcc acc | isAlpha x = tokVar xs (x:varAcc) acc
                         | otherwise = tokStr xs [x] $ (Left $ reverse varAcc):acc

tokStr :: String -> String -> [Either String String] -> [Either String String]
tokStr [] strAcc acc = reverse $ (Right $ reverse strAcc):acc
tokStr (x:xs) strAcc acc | x == '$' = case xs of
                             [] -> reverse $ (Right $ reverse strAcc):acc
                             (x':xs') | x' == '$' -> tokStr xs' (x':strAcc) acc
                                      | otherwise -> 
                                        tokVar xs "" $ (Right $ reverse strAcc):acc
                         | otherwise = tokStr xs (x:strAcc) acc

elementToExpr :: Element -> Q Exp
elementToExpr (Element name atrs cont line) =
  do
    tg <- mkTag name
    e <- [| customParent $(return tg) $
            mconcat $(fmap (ListE . catMaybes) $ mapM contentToExpr cont) |]
    addAtrs atrs e

addAtrs :: [Attr] -> Exp -> Q Exp
addAtrs [] e = return e
addAtrs ((Attr k v):t) e =
  do
    e' <- [| $(return e) ! customAttribute $(mkTag k) $(mkAttrVal [| toValue |] v) |]
    addAtrs t e'

mkAttrVal :: Q Exp -> String -> Q Exp
mkAttrVal mk v = [| mconcat $(fmap ListE $ mapM frag $ tok v) |]
  where frag (Right x) = [| $mk ($(return $ LitE $ StringL x) :: String) |]
        frag (Left x) = [| $mk $(return $ VarE $ mkName x) |]

mkTag :: QName -> Q Exp
mkTag (QName name _ pref) =
  [| stringTag $(return $ LitE $ StringL $ prefix ++ name) |]
  where prefix = maybe "" (++ ":") pref


contentToExpr :: Content -> Q (Maybe Exp)
contentToExpr (Elem c) = fmap Just $ elementToExpr c
contentToExpr (Text c) = cdataToExpr c
contentToExpr (CRef c) = fmap Just $ preEscaped $ "&" ++ c ++ ";"

cdataToExpr :: CData -> Q (Maybe Exp)
cdataToExpr (CData CDataText str _) | all isSpace str = return Nothing
                                    | otherwise = fmap Just $ mkAttrVal [| toHtml |] str
cdataToExpr (CData CDataRaw str _) = fmap Just $ preEscaped str
cdataToExpr (CData k str l) =
  error $ "Unsupported CDataKind " ++ show k ++ " " ++ show l ++ ": " ++ show str

preEscaped :: String -> Q Exp
preEscaped c = [| preEscapedToHtml ($(return $ LitE $ StringL c) :: String) |]

xml :: QuasiQuoter
xml =
  QuasiQuoter {
    quoteExp = quoteXmlExp
  , quotePat = const $ quotErr "Pattern"
  , quoteType = const $ quotErr "Type"
  , quoteDec = const $ quotErr "Declaration" }

xmlFile :: QuasiQuoter
xmlFile = quoteFile xml

quotErr :: String -> Q a
quotErr s = error $ "Cannot quote XML as " ++ s ++ "."

quoteXmlExp :: String -> Q Exp
quoteXmlExp s =
  case parseXML s of
    [] -> error "No parseable xml"
    xml ->
      do
        c <- mapM contentToExpr xml
        [| mconcat $(return $ ListE $ catMaybes c) |]

