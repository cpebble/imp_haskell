import ImpInterp
import ImpParser
import System.IO

main = do
  prog <- hGetContents stdin
  let parsed = 
        case parseImp "STDIn" prog of
         Left err -> error (show err)
         Right pimp -> pimp

  let endStore = evalCExp emptyStore parsed
   in print (endStore "out")
  --let initialStore = assignStore (assignStore emptyStore "cur" 1) "goal" 5
   --in undefined
