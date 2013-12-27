{-# LANGUAGE NoImplicitPrelude #-}
module IHaskell.Display.Charts where

import ClassyPrelude

import System.Directory
import Data.Default.Class
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Backend.Cairo
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as Char
import System.IO.Unsafe

import IHaskell.Display

instance IHaskellDisplay (Renderable a) where
  display renderable = [svg svgDisplay, png imgData]
    where 
      svgDisplay = unsafePerformIO $ chartData renderable SVG
      imgData = unsafePerformIO $ chartData renderable PNG

chartData :: Renderable a -> FileFormat -> IO String
chartData renderable format = do
  -- Switch to a temporary directory so that any files we create aren't
  -- visible. On Unix, this is usually /tmp.
  try (getTemporaryDirectory >>= setCurrentDirectory) :: IO (Either SomeException ())

  -- Write the PNG image.
  let filename = ".ihaskell-chart.png"
      opts = def{_fo_format = format}
  renderableToFile opts renderable filename

  -- Convert to base64.
  imgData <- readFile $ fpFromString filename
  return $ Char.unpack $ case format of
    PNG -> Base64.encode imgData
    _ -> imgData
