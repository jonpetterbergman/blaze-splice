{-# language RankNTypes #-}
module Text.Blaze.Load where

import Text.XML.Light.Types(Content(..),
                            Element(..),
                            CData(..),
                            QName(..),
                            Attr(..),
                            CDataKind(..))
import Text.XML.Light.Input(parseXML)  
import Text.Blaze.Internal(customParent,stringTag,customAttribute)
import Text.Blaze(Markup,AttributeValue,Tag,toValue,(!),toMarkup,preEscapedToMarkup)
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Text.IO as TIO
import Text.Blaze.Renderer.String(renderMarkup)
import Control.Monad.IO.Class(MonadIO(..))
import Control.Monad(foldM)
import System.Directory(doesFileExist)

data Macro e m =
  Macro (Map String (Macro e m) -> e -> Map String Markup -> Markup -> m Markup)

lookupAttr :: String -> Map String Markup -> Either Markup Markup
lookupAttr s mp =
  case Map.lookup s mp of
    Nothing -> Left $ toMarkup $ "Missing attribute " ++ show s
    Just v -> Right v

optionalAttr :: String -> Map String Markup -> Either Markup (Maybe Markup)
optionalAttr s mp =
  case lookupAttr s mp of
    Left e -> Right Nothing
    Right x -> Right $ Just x

withAttrs :: Monad m
          => (Map String Markup -> Either Markup a)
          -> (a -> Map String (Macro e m) -> e -> Map String Markup -> Markup -> m Markup)
          -> Macro e m
withAttrs getAttrs fn = Macro go
  where go ms e attr cont =
          case getAttrs attr of
            Left e -> return e
            Right v -> fn v ms e attr cont
  
constMacro :: Monad m => Markup -> Macro e m
constMacro x = Macro go
  where go _ _ _ _ = return x

loadFileMacro :: MonadIO m
              => FilePath
              -> Macro e m
loadFileMacro basePath = withAttrs (lookupAttr "filename") go
  where go fname macros e attrs _ = loadFile macros e $ basePath ++ "/" ++ (renderMarkup fname)

loadFile :: MonadIO m
         => Map String (Macro e m)
         -> e
         -> FilePath
         -> m Markup
loadFile ms e fn =
  do
    ex <- liftIO $ doesFileExist fn
    if ex then
      do
        txt <- liftIO $ TIO.readFile fn
        case parseXML txt of
          [] -> return $ toMarkup $ "No valid xml in " ++ show fn
          xml -> fmap mconcat $ mapM (contentToHtml ms e) xml
     else
      return $ toMarkup $ "can't load xml from " ++ show fn
      
contentToHtml :: Monad m
              => Map String (Macro e m)
              -> e
              -> Content
              -> m Markup
contentToHtml ms e (Elem c) = elementToHtml ms e c
contentToHtml _ _ (Text c) = return $ cdataToHtml c
contentToHtml _ _ (CRef c) = return $ preEscapedToMarkup $ "&" ++ c ++ ";"

cdataToHtml :: CData -> Markup
cdataToHtml (CData CDataText str _) = toMarkup str
cdataToHtml (CData CDataRaw str _) = preEscapedToMarkup str
cdataToHtml (CData k str l) =
  toMarkup $ "Unsupported CDataKind " ++ show k ++ " " ++ show l ++ ": " ++ show str


mkVal' :: Monad m
       => Map String (Macro e m)
       -> e
       -> String
       -> m Markup
mkVal' ms e ('$':'$':name) = return $ toMarkup $ '$':name
mkVal' ms e ('$':name) =
  case Map.lookup name ms of
    Nothing -> return $ toMarkup $ "Unknown macro " ++ show name
    Just (Macro m) -> m ms e mempty mempty
mkVal' ms e xs = return $ toMarkup xs


evalAttrs :: Monad m
          => Map String (Macro e m)
          -> e
          -> [Attr]
          -> m (Map String Markup)
evalAttrs ms e = foldM go mempty
  where go mp (Attr (QName k _ _) v) =
          do
            v' <- mkVal' ms e v 
            return $ Map.insert k v' mp

evalBody :: Monad m
         => Map String (Macro e m)
         -> e
         -> [Content]
         -> m (Map String Markup,Markup)
evalBody ms e = foldM go (mempty,mempty)
  where go (mp,body) (Elem c) = evalBElement ms e (mp,body) c
        go acc _ = return acc

evalBElement :: Monad m
             => Map String (Macro e m)
             -> e
             -> (Map String Markup,Markup)
             -> Element
             -> m (Map String Markup,Markup)
evalBElement ms e (mp,body) (Element (QName "body" _ Nothing) _ cont _) =
  do
    c <- fmap mconcat $ mapM (contentToHtml ms e) cont
    return $ (mp,c)
evalBElement ms e (mp,body) (Element (QName name _ Nothing) _ cont _) =
  do
    c <- fmap mconcat $ mapM (contentToHtml ms e) cont
    return $ (Map.insert name c mp,body)
evalBElement ms e (mp,body) _ = return (mp,body)

elementToHtml :: Monad m
              => Map String (Macro e m)
              -> e
              -> Element
              -> m Markup
elementToHtml macros e (Element name atrs cont line) =
  case mkMacroOrTag name of
    Left (True,macro) ->
      case Map.lookup macro macros of
        Nothing -> return $ toMarkup $ "Unknown macro" ++ show macro
        Just (Macro m) ->
          do
            (atrs',body) <- evalBody macros e cont
            m macros e atrs' body
    Left (False,macro) ->
      case Map.lookup macro macros of
        Nothing -> return $ toMarkup $ "Unknown macro " ++ show macro
        Just (Macro m) ->
          do
            c <- fmap mconcat $ mapM (contentToHtml macros e) cont
            atrs' <- evalAttrs macros e atrs
            m macros e atrs' c
    Right tag ->
      do
        cont' <- mapM (contentToHtml macros e) cont
        addAttrs macros e atrs $ customParent tag $ mconcat cont'

addAttrs :: Monad m
         => Map String (Macro e m)
         -> e
         -> [Attr]
         -> Markup
         -> m Markup
addAttrs ms _ [] el = return el
addAttrs ms e ((Attr k v):t) el =
  do
    val <- mkVal ms e v
    addAttrs ms e t $ el ! (customAttribute (mkTag k) val)

mkVal :: Monad m
      => Map String (Macro e m)
      -> e
      -> String
      -> m AttributeValue
mkVal ms _ ('$':'$':name) = return $ toValue $ '$':name
mkVal ms e ('$':name) =
  case Map.lookup name ms of
    Nothing -> return $ toValue $ "Unknown macro " ++ show name
    Just (Macro m) -> fmap (toValue . renderMarkup) $ m ms e mempty mempty
mkVal ms _ xs = return $ toValue xs

mkMacroOrTag :: QName -> Either (Bool,String) Tag
mkMacroOrTag (QName name _ (Just "macro")) = Left (False,name)
mkMacroOrTag (QName name _ (Just "longmacro")) = Left (True,name)
mkMacroOrTag qn = Right $ mkTag qn

mkTag :: QName -> Tag
mkTag (QName name _ pref) = stringTag $ prefix ++ name
  where prefix = maybe "" (++ ":") pref
