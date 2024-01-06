module Main where
import Data.Maybe
import System.Random
import Data.List (unfoldr)
import Text.Read (readMaybe)
import GHC.Base (VecElem(Int16ElemRep))
import GHC.Conc (numCapabilities)


main :: IO ()
main = do
    print "Welcome to the Universe!"
    print "if you want to quit at any point, type quit"
    print "Do you want to be a God?"
    print "yes/no"
    ans <- getLine
    case ans of
      "yes" -> heWantsToBeAGod

      "no" -> do
        heDoesntWantToBeAGod

      "quit" -> do
        print "Bye!"

      _ -> do
        print "Wrong unput!"
        main


heDoesntWantToBeAGod :: IO()
heDoesntWantToBeAGod = do
  print "Theres one lonely Higgz particle in the universe"
  let startingParticles = ["Higgz"]
  generatorNum <- getStdRandom (randomR (1, 1000000))
  let generator = randStream generatorNum
  sh <- showAll
  if sh
      then do
        startSplitting startingParticles generator 0 sh Nothing
      else do
        print "Input specific frequency to see ticks"
        input <- getLine
        print input
        let frequency = readMaybe input :: Maybe Int
        startSplitting startingParticles generator 0 sh frequency

heWantsToBeAGod :: IO()
heWantsToBeAGod = do
  print "How much particles do you want?"
  print "(Be careful, you will have to type EACH's name separately)"
  inputParticlesNum <- getLine
  if inputParticlesNum == "quit"
    then do print "bye"
    else do
      let particlesNum = readMaybe inputParticlesNum :: Maybe Int
      if isNothing particlesNum
        then do
          print "That's not a number!"
          heWantsToBeAGod
        else do
          print "Unstable particles: Higgz, W Boson, Z Boson, Top Quark"
          print "Stable particles: Any other than those above"
          startingParticles <- mapM (const getLine) [1 .. (fromJust particlesNum)]
          generatorNum <- getStdRandom (randomR (1, 1000000))
          let generator = randStream generatorNum
          sh <- showAll
          if sh
            then do
              startSplitting startingParticles generator 0 sh Nothing
            else do
              print "Input specific frequency to see ticks"
              print "(default 10000)"
              input <- getLine
              print input
              let frequency = readMaybe input :: Maybe Int
              startSplitting startingParticles generator 0 sh frequency

--make pretty
startSplitting :: [String] -> [Int] -> Int -> Bool -> Maybe Int -> IO()
startSplitting particles randStr cnt shAll frequency= do
  let newParticles = splitAll randStr particles
  if areAllStable particles
    then do
      print "The Universe is stable once again!"
    else do
      if (newParticles == particles) && not shAll
        then do
          if (cnt `rem` fromMaybe 10000 frequency) == 0
          then do
            print ("Tick #" ++ show cnt)
            print newParticles
            startSplitting newParticles (drop (length particles * 2) randStr) (cnt + 1) shAll frequency
          else
            startSplitting newParticles (drop (length particles * 2) randStr) (cnt + 1) shAll frequency
        else do
          print ("Tick #" ++ show cnt)
          print newParticles
          startSplitting newParticles (drop (length particles * 2) randStr) (cnt + 1) shAll frequency

splitPredicate :: String -> Int -> Bool
splitPredicate particle percent =
    case particle of
        "Higgz" -> percent < splitChance higgz
        "W Boson" -> percent < splitChance wBoson
        "Z Boson" -> percent < splitChance zBoson
        "Top Quark" -> percent < splitChance topQuark
        _ -> False

splitParticle :: [PartSplit] -> Int -> [String]
splitParticle [] x = ["EMPTY"]
splitParticle (x : xs) percent =
  if probability x <= percent
    then splitParticle xs percent
    else names x

splitParticles :: String -> Int -> Int -> [String]
splitParticles particle split splitPercent =
  if splitPredicate particle split
    then case particle of
      "Higgz" -> splitParticle (splits higgz) splitPercent
      "W Boson" -> splitParticle (splits wBoson) splitPercent
      "Z Boson" -> splitParticle (splits zBoson) splitPercent
      "Top Quark" -> splitParticle (splits topQuark) splitPercent
    else
      [particle]

predicate :: String -> [Int] -> [String]
predicate particle randomGen = splitParticles particle (head randomGen) (head (tail randomGen))

splitAll :: [Int] -> [String] -> [String]
splitAll randomGen [] = []
splitAll randomGen (particle : particles) = predicate particle randomGen ++ splitAll (tail (tail randomGen)) particles

areAllStable :: [String] -> Bool
areAllStable = all (`notElem` unstableParticles)

showAll :: IO Bool
showAll = do
  print "Do you want to see every tick?"
  print "yes/no"
  input <- getLine
  return (input == "yes")




--------------------------------data--------------------------------

data PartSplit = PartSplit {probability :: Int, names :: [String]} deriving (Show)
data PartInfo = PartInfo {name :: String, splitChance :: Int, splits :: [PartSplit]} deriving (Show)

randStream :: Int -> [Int]
randStream generatorNum = unfoldr (Just . uniformR (1, baseNumber)) (mkStdGen generatorNum)

baseNumber :: Int
baseNumber = 1000000 --mod 10000

unstableParticles :: [String]
unstableParticles = ["Higgz", "W Boson", "Z Boson", "Top Quark"]

higgz :: PartInfo
higgz = PartInfo "Higgz"  433 --0.0433%
   [PartSplit 216 ["Top Quark","Top Anti Quark"],
    PartSplit 460 ["Muon","Anti Muon"],
    PartSplit 1570 ["Z Boson","Photon"],
    PartSplit 3800 ["Photon","Photon"],
    PartSplit 19700 ["Z Boson","Z Boson"],
    PartSplit 52400 ["Charm Quark","Anti Charm Quark"],
    PartSplit 122800 ["Tau Lepton","Anti Tau Lepton"],
    PartSplit 211000 ["Gluon","Gluon"],
    PartSplit 352000 ["W Boson","W Boson"],
    PartSplit 1000000 ["Bottom Quark","Bottom Anti Quark"]]

wBoson :: PartInfo
wBoson = PartInfo "W Boson" 500000
   [PartSplit 333333 ["Positron", "Neutrino"],
    PartSplit 666667 ["Anti Muon", "Neutrino"],
    PartSplit 1000000 ["Anti Tau Lepton", "Neutrino"]]

zBoson :: PartInfo
zBoson = PartInfo "Z Boson" 500000
  [ PartSplit 34000 ["Electron", "Positron"],
    PartSplit 68000 ["Muon", "Anti Muon"],
    PartSplit 102000 ["Tau Lepton", "Anti Tau Lepton"],
    PartSplit 220000 ["Up Quark", "Up Anti Quark"],
    PartSplit 338000 ["Charm Quark", "Charm Anti Quark"],
    PartSplit 490000 ["Bottom Quark", "Bottom Anti Quark"],
    PartSplit 642000 ["Strange Quark", "Strange Anti Quark"],
    PartSplit 794000 ["Down Quark", "Down Anti Quark"],
    PartSplit 1000000 ["Neutrino", "Anti Neutrino"]]

topQuark :: PartInfo
topQuark = PartInfo "Top Quark" 129500
  [ PartSplit 333333 ["W Boson", "Bottom Quark"],
    PartSplit 666667 ["W Boson", "Strange Quark"],
    PartSplit 1000000 ["W Boson", "Down Quark"]]



--isStable :: String -> Bool
--isStable particle = particle `notElem` unstableParticles

--drawDouble :: Int -> Int -> IO Int
--drawDouble x y = getStdRandom (randomR (x,y))

{-
      let gen = mkStdGen 1224
      let num1 = head randStream
      let num2= head (tail randStream)
      print num1
      print num2
      let startingParticles = ["Higgz", "Higgz", "W Boson"]
      print (areAllStable startingParticles)
      print startingParticles
      let startingParticles2 = splitAll startingParticles
      print (areAllStable startingParticles2)
      print startingParticles2
      print (splitAll startingParticles2)
    -}
