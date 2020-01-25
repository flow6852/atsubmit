import System.Directory
main :: IO ()
main = getHomeDirectory >>= listDirectory >>= print
