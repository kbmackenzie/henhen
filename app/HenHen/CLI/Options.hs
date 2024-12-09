module HenHen.CLI.Options
( HenHenOptions(..)
, getOptions
) where

import HenHen (Action(..))
import HenHen.Config (LogLevel(..))
import Options.Applicative
    ( Parser
    , ParserInfo
    , CommandFields
    , Mod
    , help
    , long
    , short
    , metavar
    , switch
    , str
    , (<**>)
    , many
    , argument
    , command
    , subparser
    , fullDesc
    , progDesc
    , info
    , helper
    , execParser
    , header
    , many
    , optional
    , flag'
    , (<|>)
    )
import Data.Maybe (fromMaybe)

data HenHenOptions = HenHenOptions
    { getAction   :: Action
    , getLogLevel :: Maybe LogLevel }

makeInfo :: Parser a -> String -> ParserInfo a
makeInfo parser desc = info (parser <**> helper) (fullDesc <> progDesc desc)

logLevel :: Parser (Maybe LogLevel)
logLevel = optional (verbose <|> quiet)
    where
        verbose :: Parser LogLevel
        verbose = flag' Verbose
             ( long "verbose"
            <> short 'v'
            <> help "Enable verbose mode" )

        quiet :: Parser LogLevel
        quiet = flag' Quiet
             ( long "quiet"
            <> short 'q'
            <> help "Silence log messages" )

type ActionParser = Mod CommandFields (Maybe LogLevel -> HenHenOptions)

options :: Parser HenHenOptions
options = subparser actions <*> logLevel
    where
        actions :: ActionParser
        actions = mconcat [build, run, init_, install, uninstall, interpret, repl, copy, clean]

        build :: ActionParser
        build = command "build" $ makeInfo parser "Build project"
            where action = pure Build
                  parser = HenHenOptions <$> action

        run :: ActionParser
        run = command "run" $ makeInfo parser "Run a target or config script in virtual environment"
            where action = Run <$> name <*> args
                  name   = argument str (metavar "NAME")
                  args   = many (argument str (metavar "ARGS"))
                  parser = HenHenOptions <$> action

        init_ :: ActionParser
        init_ = command "init" $ makeInfo parser "Initialize project"
            where action = Init <$> name
                  name   = optional (argument str (metavar "NAME"))
                  parser = HenHenOptions <$> action

        install :: ActionParser
        install = command "install" $ makeInfo parser "Install dependency"
            where name   = argument str (metavar "NAME")
                  source = optional $ argument str (metavar "SOURCE")
                  action = Install <$> name <*> source
                  parser = HenHenOptions <$> action

        uninstall :: ActionParser
        uninstall = command "uninstall" $ makeInfo parser "Remove dependency"
            where name   = argument str (metavar "NAME")
                  option = switch (long "fetch" <> short 'f' <> help "Remove key from fetch map")
                  action = Uninstall <$> name <*> option
                  parser = HenHenOptions <$> action
        
        interpret :: ActionParser
        interpret = command "interpret" $ makeInfo parser "Interpret a Scheme script in virtual environment"
            where action = Interpret <$> argument str (metavar "PATH")
                  parser = HenHenOptions <$> action

        repl :: ActionParser
        repl = command "repl" $ makeInfo parser "Run the CHICKEN REPL in virtual environment"
            where parser = pure (HenHenOptions REPL)

        copy :: ActionParser
        copy = command "copy" $ makeInfo parser "Copy executable target"
            where action = Copy <$> name <*> dest
                  name   = argument str (metavar "NAME")
                  dest   = fromMaybe "." <$> optional (argument str (metavar "DESTINATION"))
                  parser = HenHenOptions <$> action

        clean :: ActionParser
        clean = command "clean" $ makeInfo parser "Clean project directory"
            where action = Clean <$> purge
                  purge  = switch (long "purge" <> short 'p' <> help "Purge virtual environment entirely")
                  parser = HenHenOptions <$> action

getOptions :: IO HenHenOptions
getOptions = execParser $ info (options <**> helper)
    ( fullDesc
   <> header "henhen - a build tool for CHICKEN Scheme" )
