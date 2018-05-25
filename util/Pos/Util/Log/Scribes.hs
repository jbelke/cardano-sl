module Pos.Util.Log.Scribes
    ( mkStdoutScribe
    , mkDevNullScribe
    ) where

import           Universum hiding (fromString)

import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO as T
import           Katip.Core
import           Katip.Format.Time (formatAsIso8601)
import           Katip.Scribes.Handle (brackets, getKeys)
import           System.IO (BufferMode (LineBuffering), IOMode (WriteMode), hFlush, hSetBuffering,
                            stdout)
import           System.IO.Unsafe (unsafePerformIO)

import           Pos.Util.Log.Internal (modifyLinesLogged)

-------------------------------------------------------------------------------
-- | create a katip scribe for stdout logging
--   (following default scribe in katip source code)

-- | global lock for stdout Scribe
{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO $ newMVar ()

mkStdoutScribe :: Severity -> Verbosity -> IO Scribe
mkStdoutScribe s v = do
    let h = stdout
        colorize = True
    hSetBuffering h LineBuffering
    let logger :: forall a. LogItem a => Item a -> IO ()
        logger item = do
          when (permitItem s item) $ bracket_ (takeMVar lock) (putMVar lock ()) $
            T.hPutStrLn h $! toLazyText $ formatItem colorize v item
    pure $ Scribe logger (hFlush h)

-- |Scribe that outputs to /dev/null without locking
mkDevNullScribe :: Severity -> Verbosity -> IO Scribe
mkDevNullScribe s v = do
    h <- openFile "/dev/null" WriteMode
    let colorize = True
    hSetBuffering h LineBuffering
    let logger :: forall a. LogItem a => Item a -> IO ()
        logger item = do
          when (permitItem s item) $ modifyLinesLogged succ
            >> (T.hPutStrLn h $! toLazyText $ formatItem colorize v item)
    pure $ Scribe logger (hFlush h)

-- | format a log item with subsecond precision (ISO 8601)
formatItem :: LogItem a => Bool -> Verbosity -> Item a -> Builder
formatItem withColor verb Item{..} =
    brackets nowStr <>
    brackets (mconcat $ map fromText $ intercalateNs _itemNamespace) <>
    brackets (fromText (renderSeverity' _itemSeverity)) <>
    brackets (fromString _itemHost) <>
    brackets (fromString (show _itemProcess)) <>
    brackets (fromText (getThreadIdText _itemThread)) <>
    mconcat ks <>
    maybe mempty (brackets . fromString . locationToString) _itemLoc <>
    fromText " " <> (unLogStr _itemMessage)
  where
    nowStr = fromText (formatAsIso8601 _itemTime)
    ks = map brackets $ getKeys verb _itemPayload
    renderSeverity' s = case s of
      EmergencyS -> red $ renderSeverity s
      AlertS     -> red $ renderSeverity s
      CriticalS  -> red $ renderSeverity s
      ErrorS     -> red $ renderSeverity s
      NoticeS    -> magenta $ renderSeverity s
      WarningS   -> yellow $ renderSeverity s
      InfoS      -> blue $ renderSeverity s
      _          -> renderSeverity s
    red = colorize "31"
    yellow = colorize "33"
    magenta = colorize "35"
    blue = colorize "34"
    colorize c s
      | withColor = "\ESC["<> c <> "m" <> s <> "\ESC[0m"
      | otherwise = s
