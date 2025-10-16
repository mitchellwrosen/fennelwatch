module Main where

import Control.Concurrent.STM (STM, TChan, atomically, check, retry)
import Control.Concurrent.STM.TChan (newTChanIO, readTChan, writeTChan)
import Control.Concurrent.STM.TVar (readTVar)
import Control.Exception (bracket_)
import Control.Monad (join)
import Data.Foldable (asum, fold, for_)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO.Utf8 qualified as Text
import GHC.Conc (registerDelay)
import Ki qualified
import System.Console.ANSI qualified as Ansi
import System.Exit (ExitCode (..))
import System.FSNotify qualified as Notify
import System.IO qualified as IO
import System.IO.Temp qualified as Temporary
import System.Process qualified as Process

main :: IO ()
main = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
  IO.hSetBuffering IO.stdout IO.NoBuffering

  eventChan <- newTChanIO

  Notify.withManager \manager -> do
    _stopListening <- Notify.watchTree manager "." (\_ -> True) (atomically . writeTChan eventChan)
    bracket_ Ansi.hideCursor Ansi.showCursor do
      bracket_ (IO.hSetEcho IO.stdout False) (IO.hSetEcho IO.stdout True) do
        Temporary.withSystemTempFile "fennelwatch" \tempPath tempHandle -> do
          Ki.scoped \scope -> do
            Ki.fork_ scope do
              Ansi.clearScreen
              Ansi.setCursorPosition 0 0
              let loop maybeLatestOutput = do
                    path <- watch eventChan
                    contents <- Text.readFile path
                    IO.hSeek tempHandle IO.AbsoluteSeek 0
                    Text.hPutStr tempHandle contents
                    IO.hFlush tempHandle
                    Process.withCreateProcess
                      (Process.proc "fennel" ["--compile", tempPath])
                        { Process.std_err = Process.CreatePipe,
                          Process.std_out = Process.CreatePipe
                        }
                      \_stdin stdoutHandle stderrHandle processHandle ->
                        Process.waitForProcess processHandle >>= \case
                          ExitSuccess -> do
                            stdout <- Text.hGetContents (fromJust stdoutHandle)
                            let output =
                                  fold
                                    [ Text.pack (Ansi.setSGRCode [Ansi.SetConsoleIntensity Ansi.BoldIntensity]),
                                      contents,
                                      Text.pack (Ansi.setSGRCode [Ansi.Reset]),
                                      "\n==>\n\n",
                                      Text.pack (Ansi.setSGRCode [Ansi.SetConsoleIntensity Ansi.BoldIntensity]),
                                      stdout,
                                      Text.pack (Ansi.setSGRCode [Ansi.Reset])
                                    ]
                            Ansi.clearScreen
                            Ansi.setCursorPosition 0 0
                            Text.putStrLn output
                            loop (Just output)
                          ExitFailure _ -> do
                            stderr <- Text.hGetContents (fromJust stderrHandle)
                            Ansi.clearScreen
                            Ansi.setCursorPosition 0 0
                            for_ maybeLatestOutput \latestOutput ->
                              Text.putStr (latestOutput <> "\n---\n\n")
                            let errput =
                                  fold
                                    [ contents,
                                      "\n==>\n\n",
                                      prettifyFennelError stderr
                                    ]
                            Text.putStrLn errput
                            loop maybeLatestOutput
              loop Nothing
            _ <- getChar
            pure ()

pattern FennelChanged :: FilePath -> Notify.Event
pattern FennelChanged path <- (asFennelChanged -> Just path)

watch :: TChan Notify.Event -> IO FilePath
watch eventChan =
  loop
  where
    loop =
      atomically (readTChan eventChan) >>= \case
        FennelChanged path0 -> loop0 path0
        _ -> loop

    loop0 path = do
      var <- registerDelay 25000
      loop1 path var

    loop1 path var = do
      select
        [ do
            fired <- readTVar var
            check fired
            pure (pure path),
          do
            readTChan eventChan >>= \case
              FennelChanged path1 -> pure (loop0 path1)
              _ -> retry
        ]

select :: [STM (IO a)] -> IO a
select =
  join . atomically . asum

asFennelChanged :: Notify.Event -> Maybe FilePath
asFennelChanged = \case
  Notify.Added path _ Notify.IsFile | isFennelFile path -> Just path
  Notify.Modified path _ Notify.IsFile | isFennelFile path -> Just path
  _ -> Nothing

isFennelFile :: FilePath -> Bool
isFennelFile =
  (".fnl" `Text.isSuffixOf`) . Text.pack

prettifyFennelError :: Text -> Text
prettifyFennelError =
  Text.unlines . dropStackTraceback [] . dropFilename . Text.lines
  where
    dropFilename = \case
      x : xs -> Text.drop 1 (Text.dropWhile (\c -> c /= ' ') x) : xs
      [] -> []

    dropStackTraceback acc = \case
      "stack traceback:" : _ -> dropWhileEmpty acc
      x : xs -> dropStackTraceback (x : acc) xs
      [] -> []

    dropWhileEmpty = \case
      "" : xs -> dropWhileEmpty xs
      xs -> reverse xs
