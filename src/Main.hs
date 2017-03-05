{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE MultiWayIf             #-}

module Main where

import ClassyPrelude
import Data.Version
import Text.XML (Node(..), Document(..), Element(..), nameLocalName, def, parseText)
import qualified Data.Map as Map
import Control.Concurrent.Async.Timer
import Control.Concurrent.Async.Lifted.Safe (poll)
import Data.Function ((&))
import System.IO (hFlush, stdout, stderr)
import System.Process.ByteString
import System.Exit
import Numeric (readHex)
import System.IO.Temp (withSystemTempDirectory)
import System.Directory
import Formatting
import Text.Regex.TDFA
import qualified Data.Text as Text
import qualified Options.Applicative as Opts
import Control.Lens
import Control.Arrow ((>>>))
import Data.List.NonEmpty (NonEmpty(..))
import System.FilePath (takeDirectory)
import Paths_dvdrip
import qualified Data.Text.Encoding as Text

data DvdInfo =
  DvdInfo { _dvdInfoTitle  :: Text
          , _dvdInfoTracks :: Map Int Track
          } deriving (Show)

data Track =
  Track { _trackTrackLength :: Double -- In seconds.
        , _trackTrackId     :: Int
        , _trackAudio       :: Map Int TrackAudio
        } deriving (Show)

data TrackAudio =
  TrackAudio { _trackAudioLanguage :: Text
             , _trackAudioStreamId :: Int
             } deriving (Show)


data DvdripEnv =
  DvdripEnv { _dvdripEnvOptions      :: DvdripOptions
            , _dvdripEnvOffset       :: Maybe Int
            , _dvdripEnvWorkingDir   :: FilePath
            , _dvdripEnvLanguages    :: Maybe [Text]
            , _dvdripEnvTracks       :: Maybe [Int]
            , _dvdripEnvAudioTrackId :: Maybe Int
            }

data DvdripOptions =
  DvdripOptions { _dvdripOptionsDvdFile        :: Maybe FilePath
                , _dvdripOptionsOutputDir      :: Maybe FilePath
                , _dvdripOptionsDvdName        :: Maybe Text
                , _dvdripOptionsTempDir        :: Maybe FilePath
                , _dvdripOptionsOffset         :: Maybe Text
                , _dvdripOptionsFormatString   :: Maybe Text
                , _dvdripOptionsLanguages      :: Maybe Text
                , _dvdripOptionsAudioTrackId   :: Maybe Text
                , _dvdripOptionsScpDestination :: Maybe Text
                , _dvdripOptionsDebugging      :: Bool
                , _dvdripOptionsAnalyze        :: Bool
                , _dvdripOptionsTracks         :: Maybe Text
                , _dvdripOptionsVersion        :: Bool
                }

data CustomFormat = CustomFormatLiteral Text
                  | CustomFormat Text
                  deriving (Eq, Show)

makeFields ''DvdripOptions
makeFields ''DvdripEnv
makeFields ''DvdInfo
makeFields ''Track
makeFields ''TrackAudio

data DvdException = DvdException Text
  deriving (Typeable)

instance Show DvdException where
  show (DvdException msg) = unpack msg

instance Exception DvdException

-- | The name of the program.
programName :: String
programName = "dvdrip"

-- | The version of the program.
programVersion :: String
programVersion = showVersion Paths_dvdrip.version

-- | Short description of the program.
programDescriptionShort :: String
programDescriptionShort = "Video DVD Ripper"

-- | Short description of the program.
programDescription :: String
programDescription = "Command line wrapper for lsdvd, dvdbackup and ffmpeg"

parseHexadecimal :: Text -> Maybe Int
parseHexadecimal s = do
  let (prefix, rest) = (take 2 s, drop 2 s)
  requireMaybe (prefix == "0x")
  case readHex (unpack rest) of
    [(n, "")] -> Just n
    _         -> Nothing
  
extractNodeElement :: MonadThrow m
                   => Node -> m Text.XML.Element
extractNodeElement (NodeElement elt) =
  return elt
extractNodeElement _ =
  throwM (DvdException "Node does not define an Element")

retrieveDvdInfo :: (MonadReader DvdripEnv m, MonadThrow m, MonadIO m)
                => FilePath -> m DvdInfo
retrieveDvdInfo inputFile = do
  let cmd  = "lsdvd"
      args = ["-a", "-Ox", inputFile]
  logDbg $ sformat ("lsdvd command: " % string % " " % string) cmd (unwords args)
  (cmdExit, cmdStdout, cmdStderr) <- liftIO $ readProcessWithExitCode cmd args ""
  when (cmdExit /= ExitSuccess) $ do
    logErr "Process exit with failure: lsdvd"
    logErr "Standard error output follows:"
    liftIO $ hPutStrLn stderr cmdStderr
  parseDvdInfo cmdStdout
  
parseDvdInfo :: (MonadReader DvdripEnv m, MonadThrow m) => ByteString -> m DvdInfo
parseDvdInfo bs = do
  -- Workaround for broken lsdvd, which sometimes returns invalid Utf8
  -- data. We simply replace invalid bytes with '?'.
  let xmlData = Text.decodeUtf8With (\ _ _ -> Just '?') bs
  Document _ root _ <- either throwM return $ parseText def (fromStrict xmlData)
  require "Node not found: lsdvd" $
    (nameLocalName . elementName) root == "lsdvd"
  let subNodes = elementNodes root
  titleNode <- maybe (throwM (DvdException "Node not found: title"))
                     return
                     (find (nodeElementWithName "title") subNodes)
  titleElt <- extractNodeElement titleNode
  titleSubNode <- maybe (throwM (DvdException "Node element does not contain sub nodes"))
                        return
                        (listToMaybe (elementNodes titleElt))
  dvdTitle <- extractNodeContent titleSubNode
  let audioTracks    = mapMaybe parseTrack subNodes
      audioTracksMap = audioTracks & map (\ track -> (track ^. trackId, track))
                         & Map.fromList
  return DvdInfo { _dvdInfoTitle  = dvdTitle
                 , _dvdInfoTracks = audioTracksMap }

parseTrack :: Node -> Maybe Track
parseTrack node@(NodeElement elt) = do
  require "Node not found: track" $
    (nameLocalName . elementName) elt == "track"
  let subNodes = elementNodes elt
  trackLengthNode <- listToMaybe $ filter (nodeElementWithName "length") subNodes
  trackLengthElt <- extractNodeElement trackLengthNode
  trackLengthS <- extractNodeContent =<< listToMaybe (elementNodes trackLengthElt)
  videoTrackLen  <- readMay trackLengthS
  videoTrackId <- readMay =<< retrieveSubnodeContentByName node "ix"
  let audioStreams    = mapMaybe parseAudioStream subNodes
      audioStreamsMap = Map.fromList $
        map (\ stream -> (stream ^. streamId, stream)) audioStreams
  return Track { _trackTrackLength = videoTrackLen
               , _trackTrackId     = videoTrackId
               , _trackAudio       = audioStreamsMap }
parseTrack _ = Nothing

retrieveSubnodeContentByName :: Node -> Text -> Maybe Text
retrieveSubnodeContentByName (NodeElement elt) nodeName = do
  node <- find (nodeElementWithName nodeName) (elementNodes elt)
  case node of
    (NodeElement elt') -> listToMaybe (elementNodes elt') >>= extractNodeContent
    _                 -> Nothing
retrieveSubnodeContentByName _ _ =
  Nothing

parseAudioStream :: Node -> Maybe TrackAudio
parseAudioStream node@(NodeElement elt) = do
  require "Node not found: audio" $
    (nameLocalName . elementName) elt == "audio"
  langCode <- retrieveSubnodeContentByName node "langcode"
  audioStreamId <- parseHexadecimal =<< retrieveSubnodeContentByName node "streamid"
  return TrackAudio { _trackAudioLanguage = langCode
                    , _trackAudioStreamId = audioStreamId }
parseAudioStream _ = Nothing

extractNodeContent :: MonadThrow m
                   => Node -> m Text
extractNodeContent (NodeContent c) =
  return c
extractNodeContent _ =
  throwM (DvdException "Node does not carry content")

nodeElementWithName :: Text -> Node -> Bool
nodeElementWithName t (NodeElement elt) =
  (nameLocalName . elementName) elt == t
nodeElementWithName _ _ = False

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right b) = Just b

require :: MonadThrow m => Text -> Bool -> m ()
require _ True = return ()
require errMsg False = throwM (DvdException errMsg)

requireMaybe :: Bool -> Maybe ()
requireMaybe True = Just ()
requireMaybe False = Nothing

listToMap :: [a] -> Map Int a
listToMap as = Map.fromList $ zip [1..] as

trackLengthThreshold :: Map Int Track -> Double
trackLengthThreshold videoTracks =
  let videoTracks' = map (view trackLength . snd) $ Map.toList videoTracks
  in case videoTracks' of
       firstTrack : restTracks -> maximum (nonNullTracks firstTrack restTracks) / 2
       [] -> 0
  where nonNullTracks :: Double -> [Double] -> NonNull [Double]
        nonNullTracks firstTrack restTracks =
          fromNonEmpty (firstTrack :| restTracks)
  
filterTracks :: Map Int Track -> Map Int Track
filterTracks videoTracks =
  let threshold = trackLengthThreshold videoTracks
  in Map.filter (trackLongerThan threshold) videoTracks
  where trackLongerThan threshold track =
          track ^. trackLength >= threshold

-- dumpTracks :: Map Int Track -> Text
-- dumpTracks videoTracks =
--   let videoTracks' = sortBy (on compare fst) $ Map.toList videoTracks
--   in concatMap dumpTrack videoTracks'

--   where dumpTrack (_, t) =
--           tshow (t ^. trackId) ++ " -> " ++ tshow (t ^. trackLength) ++ "\n"
--           ++ "Audio Streams: " ++ "\n" ++ dumpAudioStreams (t ^. audio) ++ "\n"

--         dumpAudioStreams :: Map Text TrackAudio -> Text
--         dumpAudioStreams =
--           intercalate "\n"
--           . map (\ (lang, audioStream) ->
--                     "  " ++ lang ++ " -> " ++ tshow (audioStream ^. streamId))
--           . Map.toList

prettyPrintAudioTrackLanguages :: Map Int TrackAudio -> Text
prettyPrintAudioTrackLanguages =
  Map.toList
  >>> map snd
  >>> map (\ t -> sformat (stext % "(" % stext % ")") (tshow (t ^. streamId)) (t ^. language))
  >>> intercalate ", "
  
dumpTracksCompact :: Map Int Track -> Text
dumpTracksCompact videoTracks =
  let videoTracks' = sortBy (on compare fst) $ Map.toList videoTracks
  in concatMap (dumpTrack . snd) videoTracks'

  where dumpTrack t =
          let langs = prettyPrintAudioTrackLanguages (t ^. audio)
          in sformat ("[Track #" % int % "] length = " % fixed 2 % "min" % stext % "\n")
             (t ^. trackId)
             (t ^. trackLength / 60)
             (if null langs
              then ""
              else sformat (", languages = " % stext) langs)

createCustomFormatDict :: MonadReader DvdripEnv m
                       => DvdInfo -> Int -> m (Map Text Text)
createCustomFormatDict dvd idx = do
  name <- (fromMaybe (dvd ^. title) . view (options . dvdName)) <$> ask
  return $ Map.fromList [ ("i", tshow idx)
                        , ("t", name) ]
processTrack :: (MonadIO m, MonadReader DvdripEnv m, MonadCatch m)
         => FilePath -> Text -> Track -> m ()
processTrack tmpDir outputName track = do
  logDbg $ sformat "Processing track"
  env <- ask
  inputFile <- dvdripDvdFile
  outDir <- dvdripOutputDir
  liftIO $ createDirectoryIfMissing True outDir
  ripTrack tmpDir outDir inputFile outputName' track
  case env ^. options . scpDestination of
    Just scpDest -> sendRecursively (outDir </> outputName') scpDest
    Nothing      -> return ()
  where outputName' = unpack outputName

constructTmpFileName :: FilePath -> FilePath -> FilePath
constructTmpFileName tmpDir name =
  tmpDir </> (name ++ defaultVideoSuffix)

constructOutputFileName :: FilePath -> FilePath -> FilePath
constructOutputFileName outDir name =
  outDir </> name </> (name ++ defaultVideoSuffix)

sendRecursively :: (MonadThrow m, MonadIO m) => FilePath -> Text -> m ()
sendRecursively file scpDest = do
  logMsg $ sformat ("Sending encoded file to " % stext) scpDest
  liftIO (withActivityIcon' $
           readProcessWithExitCode "scp" ["-q", "-r", file, unpack scpDest] "") >>= \case
    Right (cmdExit, _, cmdStderr) ->
      when (cmdExit /= ExitSuccess) $ do
      logErr "Failed to transmit encoded file via scp."
      logErr "Standard error output follows:"
      hPutStrLn stderr cmdStderr
      throwM (DvdException "Failed command: scp")
    Left exn -> throwM exn
  
ripTrack :: (MonadIO m, MonadReader DvdripEnv m, MonadCatch m)
         => FilePath -> FilePath -> FilePath -> FilePath -> Track -> m ()
ripTrack tmpDir outDir inputFile outputName track = do
  let tmpDirVob     = tmpDir </> ("VOB" ++ show (track ^. trackId))
      tmpOutputFile = constructTmpFileName tmpDir outputName
      outputFile    = constructOutputFileName outDir outputName
  liftIO $ do
    createDirectoryIfMissing True tmpDirVob
    createDirectoryIfMissing True (takeDirectory outDir)
  logMsg $ sformat ("Ripping track no. " % int) (track ^. trackId)
  dvdBackupRes <- try $ dvdBackup track inputFile tmpDirVob
  case dvdBackupRes of
    Right (dvdBackupExit, _, dvdBackupStderr) ->
      when (dvdBackupExit /= ExitSuccess) $ do
      logErr "Failed to retrieve VOB data from DVD, dumping stderr."
      logErr "Standard error output follows:"
      hPutStrLn stderr dvdBackupStderr
      throwM (DvdException "Failed command: dvdbackup")
    Left exn -> throwM (exn :: SomeException)
  vobFiles <- retrieveVobFiles tmpDirVob
  maybeAudioTrack <- retrieveAudioTrack track
  audioTrack <- maybe (throwM (DvdException "Failed to find audio stream"))
                      return
                      maybeAudioTrack
  logMsg $
    sformat ("Found audio stream: " % stext) (audioTrack ^. language)
  logMsg $
    sformat ("Encoding track no. " % int) (track ^. trackId)
  tracksEncodeRes <- try $ trackEncode audioTrack vobFiles tmpOutputFile
  case tracksEncodeRes of
    Right (cmdExit, _, cmdStderr) ->
      when (cmdExit /= ExitSuccess) $ do
      logErr "Failed to encode VOB data, dumping stderr."
      logErr "Standard error output follows:"
      hPutStrLn stderr cmdStderr
      throwM (DvdException "Failed command: ffmpeg")
    Left exn -> throwM (exn :: SomeException)
  logMsg $
    sformat ("Written file: " % string) outputFile
  liftIO $ renameFile tmpOutputFile outputFile

retrieveAudioTrack :: MonadReader DvdripEnv m
                   => Track -> m (Maybe TrackAudio)
retrieveAudioTrack track = do
  env <- ask
  return $ (maybe Nothing (`Map.lookup` (track ^. audio)) (env ^. audioTrackId)
             <|> maybe Nothing
                       (\ langs ->
                          (listToMaybe . filter (\a -> (a ^. language) `elem` langs) . Map.elems) (track ^. audio))
                       (env ^. languages)
             <|> (listToMaybe . map snd . sortBy (on compare fst) . Map.toList) (track ^. audio))

dvdBackup :: (MonadReader DvdripEnv m, MonadThrow m, MonadIO m)
          => Track -> FilePath -> FilePath -> m (ExitCode, ByteString, ByteString)
dvdBackup track inputFile outDir = do
  let cmd  = "dvdbackup"
      args = ["-t", show (track ^. trackId), "-i", inputFile, "-o", outDir]
  logDbg $ sformat (string % " " % string) cmd (unwords args)
  liftIO (withActivityIcon' (readProcessWithExitCode cmd args "")) >>= \case
    Right a  -> return a
    Left exn -> throwM exn

trackEncode :: (MonadIO m, MonadThrow m, MonadReader DvdripEnv m)
            => TrackAudio -> [FilePath] -> FilePath -> m (ExitCode, ByteString, ByteString)
trackEncode audioTrack inputFiles outputFile = do
  let inputFileSpec   = constructVobFilesSpec inputFiles
      audioStreamSpec = "0:i:" ++ show (audioTrack ^. streamId)
      cmd = "ffmpeg"
      args = [ "-y"
             , "-i", inputFileSpec
             , "-map", "0:v"
             , "-c:v", "libx264"
             , "-crf", "19"
             , "-preset", "slow"
             , "-map", audioStreamSpec
             , outputFile ]
  logDbg $ sformat (string % " " % string) cmd (unwords args)
  liftIO (withActivityIcon' (readProcessWithExitCode cmd args "")) >>= \case
    Right a  -> return a
    Left exn -> throwM exn

logDbg :: (MonadIO m, MonadReader DvdripEnv m) => Text -> m ()
logDbg msg =
  whenM (ask <&> view (options . debugging)) $
  hPutStrLn stderr ("[DEBUG] " ++ msg)
  
logMsg :: MonadIO m => Text -> m ()
logMsg = putStrLn

logErr :: MonadIO m => Text -> m ()
logErr = hPutStrLn stderr

constructVobFilesSpec :: [FilePath] -> String
constructVobFilesSpec files =
  "concat:" ++ intercalate "|" files

findFilesPred :: MonadIO m
              => (FilePath -> Bool) -> FilePath -> m [FilePath]
findFilesPred predicate dir = liftIO $ do
  entries <- listDirectory dir
  entries' <- mapM (\ entry -> do
                       let dirEntry = dir </> entry
                       isDir <- doesDirectoryExist dirEntry
                       if isDir
                         then findFilesPred predicate dirEntry
                         else if predicate dirEntry
                              then return [dirEntry]
                              else return []) entries
  return $ concat entries'

retrieveVobFiles :: MonadIO m => FilePath -> m [FilePath]
retrieveVobFiles dir =
  sort <$> findFilesPred isVobFile dir
  where isVobFile = (=~ (".VOB$" :: String))

runDvdrip :: (MonadMask m, MonadIO m, MonadReader DvdripEnv m) => m ()
runDvdrip = do
  opts <- view options <$> ask
  logDbg "Debugging output enabled"
  if | opts ^. Main.version -> putStrLn (pack programVersion)
     | opts ^. analyze      -> analyzeDvd
     | otherwise            -> do ripDvd
  return ()

analyzeDvd :: (MonadMask m, MonadIO m, MonadReader DvdripEnv m) => m ()
analyzeDvd = do
  inputFile <- dvdripDvdFile
  dvd <- retrieveDvdInfo inputFile
  let dvdTracks  = (dvd ^. tracks) & (filterTracks
                                      >>> Map.toList
                                      >>> map snd)
      dvdTrackIds = dvdTracks & (map (view trackId)
                                 >>> sort
                                 >>> map tshow)
  putStr $ dumpTracksCompact (dvd ^. tracks)
  putStrLn $ sformat ("Tracks to rip in automatic mode: " % stext % "\n"
                      % "If that is not desired, specify the tracks manually using `--tracks'")
                     (intercalate ", " dvdTrackIds)
  putStrLn "Selection of audio tracks for each video track:"
  forM_ dvdTracks $ \ vidTrack -> do
    retrieveAudioTrack vidTrack >>= \case
      Just t -> do
        let cmd = "mplayer"
            args = [ "-aid", tshow (t ^. streamId)
                   , "-nosub"
                   , "-dvd-device", "'" ++ pack inputFile ++ "'" -- FIXME, more robust way?
                   , "dvd://" ++ tshow (vidTrack ^. trackId) ]
        putStrLn $ sformat ("#" % stext % " -> " % stext)
                           (tshow (vidTrack ^. trackId))
                           (sformat (stext % "(" % stext % ")") (tshow (t ^. streamId)) (t ^. language))
        putStrLn $ sformat ("Preview command: " % stext)
                           (unwords (cmd:args))
      Nothing ->
        logErr "No audio track found!"

ripDvd :: (MonadMask m, MonadIO m, MonadReader DvdripEnv m) => m ()
ripDvd = do
  env <- ask
  inputFile <- dvdripDvdFile
  dvd <- retrieveDvdInfo inputFile
  baseTmpDir <- dvdripTempDir
  let dvdTracksAll = dvd ^. tracks
      tracksToRip  =  case env ^. tracks of
                        Nothing -> dvdTracksAll & filterTracks
                        Just trackIds -> foldr (\ tId accu ->
                                                   case Map.lookup tId dvdTracksAll of
                                                     Just t  -> Map.insert tId t accu
                                                     Nothing -> accu)
                                               Map.empty
                                               trackIds
      nTracksToRip = Map.size tracksToRip
      offs = fromMaybe 0 (env ^. offset)
      templateTempDir = baseTmpDir ++ "/dvdrip"
  fmtStringParsed <- parseCustomFormatString <$> dvdripFormatString (nTracksToRip > 1)
  when (nTracksToRip > 1
        && notElem (CustomFormat "i") fmtStringParsed) $
    throwM (DvdException "Multiple tracks to be ripped, but format string does not contain '%i'")
  withSystemTempDirectory templateTempDir $ \ tmpDir -> do
    logMsg $
      sformat ("Temporary directory: " % string) tmpDir
    logMsg $
      sformat ("Tracks to rip: " % stext) (tracksPretty tracksToRip)
    forM_ (zip [offs..] (map snd (Map.toList tracksToRip))) $ \ (idx, track) -> do
      formatDict <- createCustomFormatDict dvd idx
      let outputName = constructFormatted formatDict fmtStringParsed
      testOutputDirectory tmpDir outputName
      processTrack tmpDir outputName track
  logMsg "Done"
  
  where tracksPretty :: Map Int Track -> Text
        tracksPretty trackMap =
          case Map.keys trackMap of
            []        -> "(none)"
            trackKeys -> intercalate ", " . map tshow $ trackKeys
        
constructOutputName :: String -> String -> Maybe Int -> FilePath
constructOutputName name suffix maybeIdx =
  case maybeIdx of
    Just idx -> name ++ " - " ++ show idx ++ "." ++ suffix
    Nothing  -> name ++ "." ++ suffix

withActivityIcon' :: ( MonadIO m
                     , MonadBaseControl IO m
                     , Forall (Pure m)
                     ) => m a -> m (Either SomeException a)
withActivityIcon' = withActivityIcon activityAnimation
  where activityAnimation =
          ["|", "/", "—", "\\", "|", "/", "—", "\\" ]

withActivityIcon :: ( MonadIO m
                    , MonadBaseControl IO m
                    , Forall (Pure m)
                    ) => [Text] -> m a -> m (Either SomeException a)
withActivityIcon iconAnimation ma = do
  let timerConf = defaultTimerConf & timerConfSetInitDelay   0
                                   & timerConfSetInterval  150
      iconAnimationInfty = concat $ repeat iconAnimation
  withAsync ma $ \ asyncMa ->
    withAsyncTimer timerConf $ \ timer ->
      pollLoop iconAnimationInfty asyncMa timer

  where pollLoop iconAnimationInfty asyncMa timer =
          poll asyncMa >>= \case
            Nothing  ->
              do iconAnimationInfty' <- case iconAnimationInfty of
                   (icon:icons') -> do putStr (icon ++ "\r")
                                       liftIO $ hFlush stdout
                                       return icons'
                   _             -> return []
                 timerWait timer
                 pollLoop iconAnimationInfty' asyncMa timer
            Just res -> return res

parseCustomFormatString :: Text -> [CustomFormat]
parseCustomFormatString = go []
  where go acc "" = acc
        go acc t =
          let (prefix, rest) = Text.breakOn "%" t
              acc' = acc ++ if null prefix
                            then []
                            else [CustomFormatLiteral prefix]
          in if null rest
             then acc'
             else let fmtSpec = drop 1 . take 2 $ rest
                      fmtRes = case fmtSpec of
                                 "" -> []
                                 "%" -> [CustomFormatLiteral "%"]
                                 s   -> [CustomFormat s]
                      rest' = drop 2 rest
                  in go (acc' ++ fmtRes) rest'

constructFormatted :: Map Text Text -> [CustomFormat] -> Text
constructFormatted dict = concatMap evalCustomFormat
  where evalCustomFormat (CustomFormatLiteral s) = s
        evalCustomFormat (CustomFormat s) =
          fromMaybe "" (Map.lookup s dict)

defaultDvdFile :: FilePath
defaultDvdFile = "/dev/dvd"

defaultTempDir :: FilePath
defaultTempDir = "/tmp"

dvdripDvdFile :: MonadReader DvdripEnv m => m FilePath
dvdripDvdFile =
  fromMaybe defaultDvdFile . view (options . dvdFile) <$> ask
  
dvdripTempDir :: MonadReader DvdripEnv m => m FilePath
dvdripTempDir =
  fromMaybe defaultTempDir . view (options . tempDir) <$> ask

dvdripOutputDir :: MonadReader DvdripEnv m => m FilePath
dvdripOutputDir = do
  dir <- _dvdripEnvWorkingDir <$> ask
  fromMaybe dir . view (options . outputDir) <$> ask

dvdripFormatString :: MonadReader DvdripEnv m => Bool -> m Text
dvdripFormatString multiTracks = do
  let defaultFormat = if multiTracks
                      then "%t - %i"
                      else "%t"
  fromMaybe defaultFormat . view (options . formatString) <$> ask

parseOffset :: MonadThrow m => Maybe Text -> m (Maybe Int)
parseOffset Nothing = return Nothing
parseOffset (Just t) =
  case readMay t of
    Just n  -> return $ Just n
    Nothing -> throwM $ DvdException invalidOffsetMsg
  where invalidOffsetMsg = sformat ("Invalid track offset: `" % stext % "'") t

parseLanguages :: MonadThrow m => Maybe Text -> m (Maybe [Text])
parseLanguages Nothing = return Nothing
parseLanguages (Just t) =
  t & (Text.splitOn ","
       >>> map Text.strip
       >>> Just
       >>> return)

parseTracks :: MonadThrow m => Maybe Text -> m (Maybe [Int])
parseTracks Nothing = return Nothing
parseTracks (Just t) =
  t & (Text.splitOn ","
       >>> map Text.strip
       >>> map readMay
       >>> catMaybes
       >>> Just
       >>> return)

parseAudioTrackId :: MonadThrow m => Maybe Text -> m (Maybe Int)
parseAudioTrackId Nothing = return Nothing
parseAudioTrackId (Just t) =
  case readMay t of
    Just tId -> return (Just tId)
    Nothing  -> throwM (DvdException "Invalid audio track ID given")

main' :: DvdripOptions -> IO ()
main' opts = do
  dir <- getCurrentDirectory
  maybeOffset <- parseOffset (opts ^. offset)
  maybeLanguages <- parseLanguages (opts ^. languages)
  maybeTracks <- parseTracks (opts ^. tracks)
  maybeAudioTrackId <- parseAudioTrackId (opts ^. audioTrackId)
  let env = DvdripEnv { _dvdripEnvOptions = opts
                      , _dvdripEnvOffset = maybeOffset
                      , _dvdripEnvWorkingDir = dir
                      , _dvdripEnvLanguages = maybeLanguages
                      , _dvdripEnvTracks = maybeTracks
                      , _dvdripEnvAudioTrackId = maybeAudioTrackId }
  try (runReaderT runDvdrip env) >>= \case
    Right () -> return ()
    Left exn ->
      logErr $
        sformat ("Failure: " % stext) (tshow (exn :: SomeException))

dvdripOptions :: Opts.Parser DvdripOptions
dvdripOptions = DvdripOptions
  <$> (Opts.optional
       (Opts.strOption (Opts.long "input"
                        Opts.<> Opts.short 'i'
                        Opts.<> Opts.metavar "FILE"
                        Opts.<> Opts.help "Specify input file")))
  <*> (Opts.optional
       (Opts.strOption (Opts.long "output"
                        Opts.<> Opts.metavar "DIRECTORY"
                        Opts.<> Opts.help "Specify output directory")))
  <*> (fmap pack <$> Opts.optional
       (Opts.strOption (Opts.long "title"
                        Opts.<> Opts.metavar "TITLE"
                        Opts.<> Opts.help "Specify title of the dvd")))
  <*> (Opts.optional
       (Opts.strOption (Opts.long "tempdir"
                        Opts.<> Opts.metavar "DIRECTORY"
                        Opts.<> Opts.help "Specify temporary directory")))
  <*> (fmap pack <$> Opts.optional
       (Opts.strOption (Opts.long "offset"
                        Opts.<> Opts.short 'o'
                        Opts.<> Opts.metavar "NUMBER"
                        Opts.<> Opts.help "Specify track offset")))
  <*> (fmap pack <$> Opts.optional
       (Opts.strOption (Opts.long "format"
                        Opts.<> Opts.short 'f'
                        Opts.<> Opts.metavar "FORMAT STRING"
                        Opts.<> Opts.help "Specify format string")))
  <*> (fmap pack <$> Opts.optional
       (Opts.strOption (Opts.long "audio-lang"
                        Opts.<> Opts.metavar "LANGUAGE LIST"
                        Opts.<> Opts.help "Specify languages")))
  <*> (fmap pack <$> Opts.optional
       (Opts.strOption (Opts.long "audio-track"
                        Opts.<> Opts.metavar "ID"
                        Opts.<> Opts.help "Specify audio track")))
  <*> (fmap pack <$> Opts.optional
       (Opts.strOption (Opts.long "scp-to"
                        Opts.<> Opts.metavar "SCP DESTINATION"
                        Opts.<> Opts.help "Specify destination for secure copy")))
  <*> (Opts.switch
        (Opts.long "debug"
         Opts.<> Opts.help "Enable debugging output"))
  <*> (Opts.switch
       (Opts.long "analyze"
        Opts.<> Opts.help "Analyze DVD"))
  <*> (fmap pack <$> Opts.optional
       (Opts.strOption (Opts.long "tracks"
                        Opts.<> Opts.metavar "LIST OF TRACKS"
                        Opts.<> Opts.help "Specify tracks")))
  <*> (Opts.switch
       (Opts.long "version"
        Opts.<> Opts.help "Show version"))

main :: IO ()
main = main' =<<
  Opts.execParser
  (Opts.info (Opts.helper <*> dvdripOptions)
    (Opts.fullDesc
      Opts.<> Opts.progDesc programDescription
      Opts.<> Opts.header (programName ++ " - " ++ programDescriptionShort)))

defaultVideoSuffix :: FilePath
defaultVideoSuffix = ".mp4"

testOutputDirectory :: (MonadReader DvdripEnv m, MonadIO m)
                    => FilePath -> Text -> m ()
testOutputDirectory tmpDir outputName = do
  outDir <- dvdripOutputDir
  let testFileName   = constructTmpFileName tmpDir outputName'
      outputFileName = constructOutputFileName outDir outputName'
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory testFileName)
    createDirectoryIfMissing True (takeDirectory outputFileName)
    writeFile testFileName ("" :: Text)
    renameFile testFileName outputFileName
  
  where outputName' = unpack outputName
