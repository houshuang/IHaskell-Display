{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, FlexibleInstances  #-}
module IHaskell.Display.Diagrams where

import ClassyPrelude

import System.Directory
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as Char
import System.IO.Unsafe

import Diagrams.Prelude
import Diagrams.Backend.Cairo

import IHaskell.Display

instance IHaskellDisplay (Diagram Cairo R2) where
  display renderable = [png imgData]
    where 
      -- Don't send SVG so we can resize in the notebook.
      svgDisplay = unsafePerformIO $ diagramData renderable SVG
      imgData = unsafePerformIO $ diagramData renderable PNG

diagramData :: Diagram Cairo R2 -> OutputType -> IO String
diagramData renderable format = do
  -- Switch to a temporary directory so that any files we create aren't
  -- visible. On Unix, this is usually /tmp.
  try (getTemporaryDirectory >>= setCurrentDirectory) :: IO (Either SomeException ())

  -- Write the image.
  let filename = ".ihaskell-diagram." ++ extension format
  renderCairo filename (Height 250) renderable

  -- Convert to base64.
  imgData <- readFile $ fpFromString filename
  return $ Char.unpack $ case format of
    PNG -> Base64.encode imgData
    _ -> imgData
  where
    extension SVG = "svg"
    extension PNG = "png"

-- Rendering hint.
diagram :: Diagram Cairo R2 -> Diagram Cairo R2
diagram = id
