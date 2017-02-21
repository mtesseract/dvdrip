{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleContexts       #-}

module Main where

import ClassyPrelude
import Data.Version
import Text.XML
import qualified Data.Map as Map
import Control.Concurrent.Async.Timer
import Control.Concurrent.Async.Lifted.Safe (poll)
import Data.Function ((&))
import System.IO (hFlush, stdout, stderr)
import System.Process hiding (env)
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
import Paths_dvdrip

data DvdInfo =
  DvdInfo { _dvdInfoTitle  :: Text
          , _dvdInfoTracks :: Map Int Track
          } deriving (Show)

data Track =
  Track { _trackTrackLength :: Double -- In minutes.
        , _trackTrackId     :: Int
        , _trackAudio       :: Map Text TrackAudio
        } deriving (Show)

data TrackAudio =
  TrackAudio { _trackAudioLanguage :: Text
             , _trackAudioStreamId :: Int
             } deriving (Show)


data DvdripEnv =
  DvdripEnv { _dvdripEnvOptions    :: DvdripOptions
            , _dvdripEnvOffset     :: Maybe Int
            , _dvdripEnvWorkingDir :: FilePath
            , _dvdripEnvLanguages  :: Maybe [Text] }

data DvdripOptions =
  DvdripOptions { _dvdripOptionsDvdFile      :: Maybe FilePath
                , _dvdripOptionsOutputDir    :: Maybe FilePath
                , _dvdripOptionsDvdName      :: Maybe Text
                , _dvdripOptionsTempDir      :: Maybe FilePath
                , _dvdripOptionsOffset       :: Maybe Text
                , _dvdripOptionsFormatString :: Maybe Text
                , _dvdripOptionsLanguages    :: Maybe Text
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

retrieveDvdInfo :: (MonadThrow m, MonadIO m)
                => FilePath -> m DvdInfo
retrieveDvdInfo inputFile = do
  (cmdExit, cmdStdout, _) <- liftIO $
    readProcessWithExitCode "lsdvd" ["-a", "-Ox", inputFile] ""
  require "Process exit with failure: lsdvd" $
    cmdExit == ExitSuccess
  parseDvdInfo $ pack cmdStdout

parseDvdInfo :: MonadThrow m => LText -> m DvdInfo
parseDvdInfo t = do
  Document _ root _ <- either throwM return $ parseText def t
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
  let audioStreams = mapMaybe parseAudioStream subNodes
      audioStreamsMap = Map.fromList $
        map (\ stream -> (stream ^. language, stream)) audioStreams
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
  let trackLengths = map snd $ Map.toList $ Map.map (^. trackLength) videoTracks
      trackLengthTotal = sum trackLengths
      nTracks = length videoTracks
      avgTrackLength = if nTracks == 0
                       then 0
                       else trackLengthTotal / fromIntegral nTracks
  in avgTrackLength / 2

filterTracks :: Map Int Track -> Map Int Track
filterTracks videoTracks =
  let threshold = trackLengthThreshold videoTracks
  in Map.filter (trackLongerThan threshold) videoTracks
  where trackLongerThan threshold track =
          track ^. trackLength >= threshold

dumpTracks :: Map Int Track -> Text
dumpTracks videoTracks =
  let videoTracks' = sortBy (on compare fst) $ Map.toList videoTracks
  in concatMap dumpTrack videoTracks'

  where dumpTrack (_, t) =
          tshow (t ^. trackId) ++ " -> " ++ tshow (t ^. trackLength) ++ "\n"
          ++ "Audio Streams: " ++ "\n" ++ dumpAudioStreams (t ^. audio) ++ "\n"

        dumpAudioStreams :: Map Text TrackAudio -> Text
        dumpAudioStreams =
          intercalate "\n"
          . map (\ (lang, audioStream) ->
                    "  " ++ lang ++ " -> " ++ tshow (audioStream ^. streamId))
          . Map.toList

createCustomFormatDict :: MonadReader DvdripEnv m
                       => DvdInfo -> Int -> m (Map Text Text)
createCustomFormatDict dvd idx = do
  name <- (fromMaybe (dvd ^. title) . view (options . dvdName)) <$> ask
  return $ Map.fromList [ ("i", tshow idx)
                        , ("t", name) ]

ripTrack :: (MonadIO m, MonadReader DvdripEnv m, MonadThrow m)
         => FilePath -> Text -> Track -> m ()
ripTrack tmpDir outputName track = do
  inputFile <- dvdripDvdFile
  outDir <- dvdripOutputDir
  let tmpDirVob = tmpDir </> "VOB"
      outputNameSuffixed = unpack outputName ++ "." ++ suffix
      tmpOutputFile = tmpDir </> outputNameSuffixed
      outputFile = outDir </> outputNameSuffixed
  liftIO $ unlessM (doesDirectoryExist tmpDirVob) $
    createDirectory tmpDirVob
  logMsg $
    sformat ("Ripping track no. " % int) (track ^. trackId)
  dvdBackupRes <- liftIO $
    withActivityIcon' (dvdBackup track inputFile tmpDirVob)
  case dvdBackupRes of
    Right (dvdBackupExit, _, dvdBackupStderr) ->
      when (dvdBackupExit /= ExitSuccess) $ do
      logErr "Failed to retrieve VOB data from DVD, dumping stderr:"
      logErr (pack dvdBackupStderr)
      throwM (DvdException "Failed command: dvdbackup")
    Left exn -> throwM exn
  vobFiles <- retrieveVobFiles tmpDirVob
  maybeAudioTrack <- retrieveAudioTrack track
  audioTrack <- maybe (throwM (DvdException "Failed to find audio stream"))
                      return
                      maybeAudioTrack
  logMsg $
    sformat ("Found audio stream: " % stext) (audioTrack ^. language)
  logMsg $
    sformat ("Encoding track no. " % int) (track ^. trackId)
  tracksEncodeRes <- liftIO $
    withActivityIcon' (trackEncode audioTrack vobFiles tmpOutputFile)
  case tracksEncodeRes of
    Right (cmdExit, _, cmdStderr) ->
      when (cmdExit /= ExitSuccess) $ do
      logErr "Failed to encode VOB data, dumping stderr:"
      logErr (pack cmdStderr)
      throwM (DvdException "Failed command: ffmpeg")
    Left exn -> throwM exn
  logMsg $
    sformat ("Written file: " % string) outputFile
  liftIO $ renameFile tmpOutputFile outputFile

  where suffix = "mkv"

retrieveAudioTrack :: MonadReader DvdripEnv m
                   => Track -> m (Maybe TrackAudio)
retrieveAudioTrack track = ask <&>
  (view languages
   >>> fromMaybe []
   >>> map (`Map.lookup` (track ^. audio))
   >>> catMaybes
   >>> listToMaybe)

dvdBackup :: MonadIO m
          => Track -> FilePath -> FilePath -> m (ExitCode, String, String)
dvdBackup track inputFile outDir = liftIO $
  readProcessWithExitCode
    "dvdbackup" ["-t", show (track ^. trackId), "-i", inputFile, "-o", outDir] ""

trackEncode :: MonadIO m
            => TrackAudio -> [FilePath] -> FilePath -> m (ExitCode, String, String)
trackEncode audioTrack inputFiles outputFile = liftIO $ do
  let inputFileSpec = constructVobFilesSpec inputFiles
      audioStreamSpec = "0:i:" ++ show (audioTrack ^. streamId)
  readProcessWithExitCode
    "ffmpeg" [ "-y"
             , "-i", inputFileSpec
             , "-map", "0:v"
             , "-c:v", "libx264"
             , "-map", audioStreamSpec
             , outputFile ] ""

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
retrieveVobFiles =
  findFilesPred isVobFile
  where isVobFile = (=~ (".VOB$" :: String))

ripDvd :: (MonadMask m, MonadIO m, MonadReader DvdripEnv m) => m ()
ripDvd = do
  env <- ask
  inputFile <- dvdripDvdFile
  dvd <- retrieveDvdInfo inputFile
  baseTmpDir <- dvdripTempDir
  let dvdTracks  = filterTracks (dvd ^. tracks)
      dvdTracks' = map snd . sortBy (on compare fst) . Map.toList $ dvdTracks
      nDvdTracks = length dvdTracks'
      offs = fromMaybe 0 (env ^. offset)
      templateTempDir = baseTmpDir ++ "/dvdrip"
    
  fmtStringParsed <- parseCustomFormatString <$> dvdripFormatString dvd

  when (nDvdTracks > 1
        && notElem (CustomFormat "i") fmtStringParsed) $
    throwM (DvdException "Multiple tracks to be ripped, but format string does not contain '%i'")

  withSystemTempDirectory templateTempDir $ \ tmpDir -> do

    logMsg $
      sformat ("Temporary directory: " % string) tmpDir
    logMsg $
      sformat ("Tracks to rip: " % stext) (tracksPretty dvdTracks)
    forM_ (zip [offs..] dvdTracks') $ \ (idx, track) -> do
      formatDict <- createCustomFormatDict dvd idx
      let outputName = constructFormatted formatDict fmtStringParsed
      ripTrack tmpDir outputName track

  where tracksPretty :: Map Int Track -> Text
        tracksPretty =
          intercalate ", " . map tshow . Map.keys

        
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

dvdripFormatString :: MonadReader DvdripEnv m => DvdInfo -> m Text
dvdripFormatString dvd = do
  let multiTracks = length (_dvdInfoTracks dvd) > 1
      defaultFormat = if multiTracks
                      then "%t - %i"
                      else "%t"
  fromMaybe defaultFormat . view (options . formatString) <$> ask

parseOffset :: MonadThrow m => Maybe Text -> m (Maybe Int)
parseOffset Nothing = return Nothing
parseOffset (Just t) =
  case readMay t of
    Just n  -> return $ Just n
    Nothing -> throwM $
      DvdException (sformat ("Invalid track offset: `" % stext % "'") t)

parseLanguages :: MonadThrow m => Maybe Text -> m (Maybe [Text])
parseLanguages Nothing = return Nothing
parseLanguages (Just t) =
  t & (Text.splitOn ","
       >>> map Text.strip
       >>> map (`Map.lookup` languageDb)
       >>> catMaybes
       >>> concat
       >>> Just
       >>> return)

main' :: DvdripOptions -> IO ()
main' opts = do
  dir <- getCurrentDirectory
  maybeOffset <- parseOffset (opts ^. offset)
  maybeLanguages <- parseLanguages (opts ^. languages)
  let env = DvdripEnv { _dvdripEnvOptions = opts
                      , _dvdripEnvOffset = maybeOffset
                      , _dvdripEnvWorkingDir = dir
                      , _dvdripEnvLanguages = maybeLanguages }
  try (runReaderT ripDvd env) >>= \case
    Right () -> logMsg "Done"
    Left exn ->
      logErr $
        sformat ("Failure: " % stext) (tshow (exn :: SomeException))

-- | The name of the program.
programName :: String
programName = "dvdrip"

-- | The version of the program.
programVersion :: String
programVersion = showVersion version

-- | Short description of the program.
programDescriptionShort :: String
programDescriptionShort = "Video DVD Ripper"

-- | Short description of the program.
programDescription :: String
programDescription = "Command line wrapper for lsdvd, dvdbackup and ffmpeg"

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

languageDb :: Map Text [Text]
languageDb = Map.fromList
  [ ("de", ["de", "DE", "ger", "GERMAN"])
  , ("en", ["en", "EN", "english", "ENGLISH"])
  ]

main :: IO ()
main = main' =<<
  Opts.execParser
  (Opts.info (Opts.helper <*> dvdripOptions)
    (Opts.fullDesc
      Opts.<> Opts.progDesc programDescription
      Opts.<> Opts.header (programName ++ " - " ++ programDescriptionShort)))
