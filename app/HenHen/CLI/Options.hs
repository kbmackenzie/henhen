module HenHen.CLI.Options
( HenHenCommand(..)
, getCommand
) where

import HenHen (Action(..))
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
    )
import Data.Maybe (fromMaybe)

data HenHenCommand = HenHenCommand
    { commandAction  :: Action
    , commandQuiet   :: Bool
    , commandVerbose :: Bool }

makeInfo :: Parser a -> String -> ParserInfo a
makeInfo parser desc = info (parser <**> helper) (fullDesc <> progDesc desc)

parseAction :: Parser Action
parseAction = subparser . mconcat $ [build, run, init_, install, interpret, copy, clean]
    where
        build :: Mod CommandFields Action
        build = command "build" $ makeInfo (pure Build) "Build project"

        run :: Mod CommandFields Action
        run = command "run" $ makeInfo parser "Run binary or script in virtual environment"
            where parser = Run <$> name <*> args
                  name   = argument str (metavar "NAME")
                  args   = many (argument str (metavar "ARGS"))

        init_ :: Mod CommandFields Action
        init_ = command "init" $ makeInfo parser "Initialize project"
            where parser = Init <$> name
                  name   = fromMaybe "unnamed" <$> optional (argument str (metavar "NAME"))

        install :: Mod CommandFields Action
        install = command "install" $ makeInfo parser "Install dependency"
            where parser = Install <$> argument str (metavar "NAME")

        
        interpret :: Mod CommandFields Action
        interpret = command "interpret" $ makeInfo parser "Interpret script in virtual environment"
            where parser = Interpret <$> argument str (metavar "PATH")

        copy :: Mod CommandFields Action
        copy = command "copy" $ makeInfo parser "Copy executable target"
            where parser = Copy <$> name <*> dest
                  name   = argument str (metavar "NAME")
                  dest   = fromMaybe "." <$> optional (argument str (metavar "DESTINATION"))

        clean :: Mod CommandFields Action
        clean = command "clean" $ makeInfo parser "Clean project directory"
            where parser = Clean <$> purge
                  purge  = switch (long "purge" <> short 'p' <> help "Purge virtual environment entirely")

parseCommand :: Parser HenHenCommand
parseCommand = HenHenCommand <$> parseAction <*> quiet <*> verbose
    where quiet   = switch (long "quiet"   <> short 'q' <> help "Silence log messages")
          verbose = switch (long "verbose" <> short 'v' <> help "Enable verbose mode" )

getCommand :: IO HenHenCommand
getCommand = execParser $ info (parseCommand <**> helper)
    ( fullDesc
   <> header "henhen - a build tool for CHICKEN Scheme" )
