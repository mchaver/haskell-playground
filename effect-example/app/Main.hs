-- Language extensions required by Effectful's dynamic-dispatch machinery:
--   DataKinds        promotes the effect/list types so 'Eff (FileSystem : es)'
--                    and the kind 'Effect' are usable at the type level.
--   GADTs            lets the 'FileSystem' effect be a GADT whose constructors
--                    (ReadFile, WriteFile) carry their own result types.
--   TypeFamilies     needed for the 'DispatchOf FileSystem = Dynamic' and
--                    'type instance' declarations.
--   TypeOperators    allows the infix ':>' and ':' operators in signatures
--                    (e.g. 'FileSystem :> es', 'FileSystem : es').
--   FlexibleContexts permits non-type-variable constraints like 'FileSystem :> es'.
--   TypeApplications enables the '@FsError' syntax used to pick which error
--                    type 'runError' should handle.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Effectful (Dispatch(Dynamic), DispatchOf, Eff, Effect, IOE, (:>), liftIO, runEff, runPureEff)
import Effectful.Error.Static (Error, prettyCallStack, runError, throwError)
import Effectful.Exception (catchIO)
import Effectful.Dispatch.Dynamic (interpret, reinterpret, send)
import Effectful.State.Static.Local (get, modify, runState)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified System.IO as IO

-- send: execute effect operation

newtype FsError = FsError String deriving Show

-- | A dynamically-dispatched effect describing the operations we can perform
-- against a file system. The constructors are the "API"; how they are actually
-- carried out is decided later by an interpreter.
data FileSystem :: Effect where
  ReadFile  :: FilePath -> FileSystem m String
  WriteFile :: FilePath -> String -> FileSystem m ()

type instance DispatchOf FileSystem = Dynamic

-- convert data constructor to functions with the send

-- Smart constructors: lift each operation into the 'Eff' monad with 'send'.
-- These are what user code actually calls.
readFile' :: (FileSystem :> es) => FilePath -> Eff es String
readFile' path = send (ReadFile path)

writeFile' :: (FileSystem :> es) => FilePath -> String -> Eff es ()
writeFile' path contents = send (WriteFile path contents)

-- perform an IO action for each con
-- | A real interpreter: perform each operation in 'IO', converting any IO
-- exception into a typed 'FsError'.
runFileSystemIO
  :: (IOE :> es, Error FsError :> es)
  => Eff (FileSystem : es) a
  -> Eff es a
runFileSystemIO = interpret $ \_ eff ->
  case eff of
    ReadFile path           -> adapt $ IO.readFile path
    WriteFile path contents -> adapt $ IO.writeFile path contents
  where
    adapt m = liftIO m `catchIO` \e -> throwError . FsError $ show e

-- | A second, completely separate effect: structured logging. It exists to show
-- that effects are scoped *independently of one another* — holding 'FileSystem'
-- grants no power to log, and holding 'Logger' grants no power to touch files.
data Logger :: Effect where
  LogMsg :: String -> Logger m ()

type instance DispatchOf Logger = Dynamic

logMsg :: (Logger :> es) => String -> Eff es ()
logMsg msg = send (LogMsg msg)

-- | Interpret logging by printing to stdout.
runLoggerIO :: (IOE :> es) => Eff (Logger : es) a -> Eff es a
runLoggerIO = interpret $ \_ (LogMsg msg) -> liftIO (putStrLn ("[log] " <> msg))

-- perform non io actions when interpreting the effect system
-- | A pure interpreter: instead of touching the disk, it carries an in-memory
-- @Map FilePath String@ as a private 'State' effect. 'reinterpret' lets the
-- handler introduce that local 'State' which it then discharges with 'runState',
-- so callers never see it. Great for tests — no IO required.
runFileSystemPure
  :: (Error FsError :> es)
  => Map FilePath String
  -> Eff (FileSystem : es) a
  -> Eff es (a, Map FilePath String)
runFileSystemPure fs0 = reinterpret (runState fs0) $ \_ eff ->
  case eff of
    ReadFile path -> do
      fs <- get
      case Map.lookup path fs of
        Just contents -> pure contents
        Nothing       -> throwError . FsError $ "no such file: " <> path
    WriteFile path contents -> modify (Map.insert path contents)

-- | A program written against the abstract 'FileSystem' effect. It has no idea
-- whether it is talking to a real disk or something else.
program :: (FileSystem :> es) => Eff es String
program = do
  writeFile' "/tmp/effectful-example.txt" "Hello from Effectful!\n"
  readFile' "/tmp/effectful-example.txt"

-- Effects are limited in scope by the constraints in the type signature: a
-- function may ONLY perform effects listed in its 'es'. Crucially this is
-- enforced at COMPILE time, not at runtime — code that reaches for an effect it
-- wasn't granted simply will not build.
--
-- 'program' above is constrained to @FileSystem :> es@ and nothing else. Even
-- though we have a perfectly good 'Logger' effect, 'program' has not asked for
-- it, so it cannot log. Adding this line inside 'program'...
--
--   logMsg "this should not type-check"
--
-- ...makes GHC reject the whole module:
--
--   error: [GHC-39999]
--       • Could not deduce ‘Logger :> es’ arising from a use of ‘logMsg’
--         from the context: FileSystem :> es
--
-- The only way to make it legal is to widen the contract so the capability is
-- advertised in the type, as 'verboseProgram' does below. The type signature is
-- thus an exhaustive, machine-checked list of everything a function can do.

-- | Same work as 'program', but it also logs. Because it uses two effects it
-- must declare both — and every caller now sees, from the type alone, that this
-- routine both touches the file system and logs.
verboseProgram :: (FileSystem :> es, Logger :> es) => Eff es String
verboseProgram = do
  logMsg "writing greeting"
  writeFile' "/tmp/effectful-example.txt" "Hello from Effectful!\n"
  logMsg "reading it back"
  readFile' "/tmp/effectful-example.txt"

main :: IO ()
main = do
  -- Same 'program', interpreted against the real file system in IO.
  putStrLn "== runFileSystemIO (real disk) =="
  ioResult <- runEff . runError @FsError . runFileSystemIO $ program
  report ioResult

  -- Same 'program', interpreted purely against an in-memory Map. No IO at all:
  -- it runs inside 'runPureEff'. We drop the final Map with 'fst'.
  putStrLn "\n== runFileSystemPure (in-memory) =="
  let pureResult = runPureEff . runError @FsError . runFileSystemPure Map.empty $ program
  report (fmap fst pureResult)

  -- 'verboseProgram' needs two effects, so we satisfy both by stacking their
  -- interpreters. Order is free; each interpreter discharges one effect.
  putStrLn "\n== verboseProgram (FileSystem + Logger) =="
  vResult <- runEff . runError @FsError . runLoggerIO . runFileSystemIO $ verboseProgram
  report vResult
  where
    report res = case res of
      Left (callStack, FsError err) ->
        putStrLn $ "File system error: " <> err <> "\n" <> prettyCallStack callStack
      Right contents ->
        putStr $ "Read back:\n" <> contents
