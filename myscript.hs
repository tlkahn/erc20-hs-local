    import Shower (printer)
    import System.Environment (getEnv)

    main :: IO ()
    main = do
      let question = "The answer to life the universe and everything or whatever"
      answer <- getEnv "ANSWER"
      printer (question, "is", answer)
