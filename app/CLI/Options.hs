module CLI.Options
( getAction
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
    )

makeInfo :: Parser a -> String -> ParserInfo a
makeInfo parser desc = info (parser <**> helper) (fullDesc <> progDesc desc)

parseAction :: Parser Action
parseAction = subparser . mconcat $ [build, run, init_, install, interpret, clean]
    where
        build :: Mod CommandFields Action
        build = command "build" $ makeInfo (pure Build) "Build project"

        run :: Mod CommandFields Action
        run = command "run" $ makeInfo parser "Run binary or script in virtual environment"
            where parser = Run
                    <$> argument str (metavar "NAME")
                    <*> many (argument str (metavar "ARG"))

        init_ :: Mod CommandFields Action
        init_ = command "init" $ makeInfo parser "Initialize project"
            where parser = Init
                    <$> argument str (metavar "NAME")

        install :: Mod CommandFields Action
        install = command "install" $ makeInfo parser "Install dependency"
            where parser = Install
                    <$> argument str (metavar "NAME")

        
        interpret :: Mod CommandFields Action
        interpret = command "interpret" $ makeInfo parser "Interpret script in virtual environment"
            where parser = Interpret
                    <$> argument str (metavar "PATH")

        clean :: Mod CommandFields Action
        clean = command "clean" $ makeInfo parser "Clean project directory"
            where parser = Clean
                    <$> switch
                        ( long "purge"
                       <> short 'p'
                       <> help "Purge virtual environment entirely" )

getAction :: IO Action
getAction = execParser $ info (parseAction <**> helper)
    ( fullDesc
   <> header "henhen - a build tool for CHICKEN Scheme" )
