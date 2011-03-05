module Codeforces ( contests
                  , mySubmissions
                  , registerForPractice
                  , contestProblem
                  , contestSubmit  
                  , Contest (..)
                  , Lang (..)
                  ) where

import Control.Monad (unless)
import qualified Data.ByteString.Char8 as B
import Data.List (isSuffixOf)
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.Time.Format (readTime)
import Network.HTTP ( RequestMethod(POST), Response(..), defaultGETRequest )
import Network.URI  (parseURI, URI (..))
import Network.Browser ( BrowserAction, Form(Form), Cookie
                       , browse, formToRequest, request, ioAction, setCookies, getCookies
                       , setAllowRedirects)
import Network.TCP ( HandleStream )  
import System.Locale ( defaultTimeLocale )
import Text.HTML.Yuuko (yuuko)

data Contest = Contest { contestID :: Maybe Int
                       , contestName :: String
                       }
             deriving Show

data Submission = Submission { submissionID :: Int
                             , submissionWhen :: UTCTime
                             , submissionPerson :: String
                             , submissionProblem :: String
                             , submissionLang :: String
                             , submissionVerdict :: String
                             , submissionTime :: Int
                             , submissionMemory :: Int
                             }
                  deriving Show
data Lang = GNUCpp
          | MSVCpp
          | Delphi7
          | FreePascal2
          | Java6
          | PHP52plus
          | Python26plus
          | Ruby17
          | CSharpMono26plus
          | GNUC4
          | FSharp20
          | HaskellGHC612
          deriving Show

------------------------------------------------------------
type Handle = String
type Password = String
type ContestNumber = Int
type QuizSection = String

loginURI :: URI
loginURI = fromJust $ parseURI "http://www.codeforces.com/enter"

loginCookieFile :: FilePath
loginCookieFile = "codeforces.cookie.txt"

loadCookies :: BrowserAction (HandleStream String) [Cookie]
loadCookies = do
  res <- ioAction $ catch
         (do
             cont <- readFile loginCookieFile
             return $ read cont)
         (const $ return [])
  ioAction $ return res
  
login :: Handle -> Password -> BrowserAction (HandleStream String) Bool
login handle password = do
  cks <- loadCookies
  setCookies cks
  setAllowRedirects True
  (uri, rsp) <- request $ defaultGETRequest loginURI
  let contents = rspBody rsp
      yuuko' = flip yuuko contents
      hiddens = zip (yuuko' "//form//input[@type='hidden']/@name")
                    (yuuko' "//form//input[@type='hidden']/@value")
      params = hiddens ++ defaultParams
      form = Form POST loginURI params
  if (uriPath uri == "/enter") 
    then reallyLogin form
    else alreadyLogin
  where defaultParams = [ ("handle", handle)
                        , ("password", password)
                        ]
        reallyLogin form = do
          (uri, _) <- request $ formToRequest form
          saveCookies
          unless (uriPath uri /= "/enter") $ error "login failed"
          ioAction $ return True
        saveCookies = do
          ncks <- getCookies
          ioAction $ catch (writeFile loginCookieFile (show ncks)) (const $ return ())
        alreadyLogin = do
          saveCookies
          ioAction $ return True

------------------------------------------------------------
contestsURI :: URI
contestsURI = fromJust $ parseURI "http://www.codeforces.com/contests"
contests :: IO [Contest]
contests = do
  (_, rsp) <- Network.Browser.browse $ do
    request $ defaultGETRequest contestsURI
  let contents = rspBody rsp
      datasets = map (yuuko "/td") $ yuuko "//div[@class='datatable']//tr" contents
      nameandids = map (concat . lines) $ concat $ map (take 1) datasets
      names = map clenup nameandids
      ids = map hrefid nameandids
  return $ zipWith Contest ids names
  where clenup = B.unpack . trim . firstline . B.pack
        firstline str = fst $ B.breakSubstring (B.pack "<br/>") str
        hrefid = hrefclean . (yuuko "/a/@href")
        hrefclean [] = Nothing
        hrefclean (s:_) = (fmap read) . (fmap B.unpack) . getid $ B.pack s
        getid str
          | B.null str = Nothing
          | contentsPrefix `B.isPrefixOf` str = Just $ B.drop (B.length contentsPrefix) str
          | otherwise = error "contents href format error"
        contentsPrefix = B.pack "/contest/"

------------------------------------------------------------
registerForPracticeURI :: ContestNumber -> URI
registerForPracticeURI n = fromJust . parseURI $ "http://www.codeforces.com/contest/" ++ (show n)
registerForPractice :: Handle -> Password -> Int -> IO Bool
registerForPractice handle password cid = do
  (_, rsp) <- Network.Browser.browse $ do
    login handle password
    request $ formToRequest form
  return $ registered `B.isInfixOf` (B.pack $ rspBody rsp)
  where form = Form POST (registerForPracticeURI cid) [("action", "registerForPractice")]
        registered = B.pack "You are registered for practice."

------------------------------------------------------------
mySubmissionsURI :: ContestNumber -> URI
mySubmissionsURI n = fromJust . parseURI $ 
                     "http://www.codeforces.com/contest/" ++ (show n) ++ "/my"
mySubmissions :: Handle -> Password -> ContestNumber -> IO [Submission]
mySubmissions handle password cid = do
  (_, rsp) <- Network.Browser.browse $ do
    login handle password
    request . defaultGETRequest $ mySubmissionsURI cid
  let contents = rspBody rsp
      datasets = map (yuuko "/td") $ tail $ yuuko "//div[@class='datatable']//tr" contents
  return $ map (subm.extract) datasets
  where extract xs = zipWith ($) cleanFuncs (map basicClean xs)
        basicClean = trimStr . concat . lines
        cleanFuncs = [ head . (yuuko "/a/@submissionid")
                     , id
                     , head . (yuuko "/a")
                     , trimStr . head . (yuuko "/a")
                     , id
                     , cleanVerdict
                     , cleanTime
                     , cleanMemory
                     ]
        cleanVerdict s = head . concat $ map ($ s) (map yuuko ["/a", "/span"])
        cleanTime s
          | " ms"  `isSuffixOf` s = take (length s - 3) s
          | otherwise = error "submission time format error"
        cleanMemory s
          | " KB"  `isSuffixOf` s = take (length s - 3) s
          | otherwise = error "submission memory format error"
        subm (s1:s2:s3:s4:s5:s6:s7:s8:[]) = 
          Submission { submissionID = read s1
                     , submissionWhen = addUTCTime (-(3 * 60 * 60)) $ dateConv s2
                     , submissionPerson = s3
                     , submissionProblem = s4
                     , submissionLang = s5
                     , submissionVerdict = s6
                     , submissionTime = read s7
                     , submissionMemory = read s8
                     }
        subm _ = error "subm match error"
        dateConv = readTime defaultTimeLocale "%b %d, %Y %l:%M:%S %P"

------------------------------------------------------------
contestProblemURI :: ContestNumber -> QuizSection -> URI
contestProblemURI n x = fromJust . parseURI $
                        "http://www.codeforces.com/contest/" ++ show n ++ "/problem/" ++ x
contestProblem :: ContestNumber -> QuizSection -> IO String
contestProblem n x = do
  (_, rsp) <- Network.Browser.browse $ do
    request . defaultGETRequest $ contestProblemURI n x
  let contents = rspBody rsp
  return . head $ yuuko "//div[@class='problem-statement']" contents

------------------------------------------------------------
contestSubmmitURI :: ContestNumber -> URI
contestSubmmitURI n = fromJust . parseURI $
                        "http://www.codeforces.com/contest/" ++ show n ++ "/submit"
contestSubmit :: Handle -> Password -> ContestNumber -> QuizSection -> Lang -> FilePath -> IO Bool
contestSubmit handle password cid section lang file = do
  contents <- readFile file
  (_, rsp) <- Network.Browser.browse $ do
    login handle password
    request . formToRequest $ form contents
  return $ solutionSent `B.isInfixOf` (B.pack $ rspBody rsp)
  where form contents = Form POST (contestSubmmitURI cid) $ params contents
        params contents = [ ("action", "submitSolutionFormSubmitted")
                          , ("submittedProblemShortName", section)
                          , ("language", langNumString lang)  
                          , ("source", contents)
                          , ("sourceFile", "")
                          , ("submit", "Submit")
                          ]
        solutionSent = B.pack $ "Solution to the problem " ++ section ++ 
                       " has been submitted successfully"

------------------------------------------------------------
trim :: B.ByteString -> B.ByteString
trim = trimleft . trimright
trimleft :: B.ByteString -> B.ByteString
trimleft = B.dropWhile (== ' ')
trimright :: B.ByteString -> B.ByteString
trimright = B.reverse . (B.dropWhile (== ' ')) . B.reverse
trimStr :: String -> String
trimStr = B.unpack . trim . B.pack

langNumString :: Lang -> String
langNumString = show . langNumber

langNumber :: Lang -> Int
langNumber GNUCpp           = 1
langNumber MSVCpp           = 2
langNumber Delphi7          = 3
langNumber FreePascal2      = 4
langNumber Java6            = 5
langNumber PHP52plus        = 6
langNumber Python26plus     = 7
langNumber Ruby17           = 8
langNumber CSharpMono26plus = 9
langNumber GNUC4            = 10
langNumber FSharp20         = 11
langNumber HaskellGHC612    = 12
