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

parseCommand :: Parser HenHenCommand
parseCommand = subparser actions
    where
        actions :: Mod CommandFields HenHenCommand
        actions = mconcat [build, run, init_, install, interpret, repl, copy, clean]

        quiet :: Parser Bool
        quiet = switch (long "quiet"   <> short 'q' <> help "Silence log messages")

        verbose :: Parser Bool
        verbose = switch (long "verbose" <> short 'v' <> help "Enable verbose mode" )

        build :: Mod CommandFields HenHenCommand
        build = command "build" $ makeInfo parser "Build project"
            where action = pure Build
                  parser = HenHenCommand <$> action <*> quiet <*> verbose

        run :: Mod CommandFields HenHenCommand
        run = command "run" $ makeInfo parser "Run binary or script in virtual environment"
            where action = Run <$> name <*> args
                  name   = argument str (metavar "NAME")
                  args   = many (argument str (metavar "ARGS"))
                  parser = HenHenCommand <$> action <*> quiet <*> verbose

        init_ :: Mod CommandFields HenHenCommand
        init_ = command "init" $ makeInfo parser "Initialize project"
            where action = Init <$> name
                  name   = fromMaybe "unnamed" <$> optional (argument str (metavar "NAME"))
                  parser = HenHenCommand <$> action <*> quiet <*> verbose

        install :: Mod CommandFields HenHenCommand
        install = command "install" $ makeInfo parser "Install dependency"
            where action = Install <$> argument str (metavar "NAME")
                  parser = HenHenCommand <$> action <*> quiet <*> verbose

        
        interpret :: Mod CommandFields HenHenCommand
        interpret = command "interpret" $ makeInfo parser "Interpret script in virtual environment"
            where action = Interpret <$> argument str (metavar "PATH")
                  parser = HenHenCommand <$> action <*> quiet <*> verbose

        repl :: Mod CommandFields HenHenCommand
        repl = command "repl" $ makeInfo parser "Run repl"
            where parser = HenHenCommand REPL <$> quiet <*> verbose

        copy :: Mod CommandFields HenHenCommand
        copy = command "copy" $ makeInfo parser "Copy executable target"
            where action = Copy <$> name <*> dest
                  name   = argument str (metavar "NAME")
                  dest   = fromMaybe "." <$> optional (argument str (metavar "DESTINATION"))
                  parser = HenHenCommand <$> action <*> quiet <*> verbose

        clean :: Mod CommandFields HenHenCommand
        clean = command "clean" $ makeInfo parser "Clean project directory"
            where action = Clean <$> purge
                  purge  = switch (long "purge" <> short 'p' <> help "Purge virtual environment entirely")
                  parser = HenHenCommand <$> action <*> quiet <*> verbose

getCommand :: IO HenHenCommand
getCommand = execParser $ info (parseCommand <**> helper)
    ( fullDesc
   <> header "henhen - a build tool for CHICKEN Scheme" )
