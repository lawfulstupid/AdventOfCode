module AdventOfCode.Y2021.Day19 where

import AdventOfCode.Common.Vector3
import AdventOfCode.Common.List

import Control.Monad

import Data.List
import Data.Maybe

--------------------------------------------------------------------------------

data Scanner = Scanner
   { idNum :: Int
   , beacons :: Beacons
   , diffSet :: [Vector Int]
   } deriving (Show)

type KnownBeacons = Scanner

printScanner :: Scanner -> IO ()
printScanner (Scanner n vs _) = do
   putStrLn ("--- scanner " ++ show n ++ " ---")
   forM_ vs (putStrLn . init . tail . show)
   putStrLn ""

type Beacons = [Vector Int]

--------------------------------------------------------------------------------

applyLT :: (Vector Int -> Vector Int) -> Scanner -> Scanner
applyLT t (Scanner n vs ds) = Scanner n (map t vs) (map t ds)

getRotations :: Scanner -> [Scanner]
getRotations s = [applyLT (apply r) s | r <- rotations]

diffs :: Beacons -> [Vector Int]
diffs vs = let
   vs' = sort vs
   in do
      (u:vs'') <- tails vs'
      v <- vs''
      return (u - v)

-- Given a set of "true" beacon locations and a rotated scanner,
-- determine if the scanner readings can be translated to overlap the known beacons
hasTranslation :: KnownBeacons -> Scanner -> Bool
hasTranslation known scanner = hasLength 66 $ intersect (diffs$beacons known) (diffs$beacons scanner)

-- Given a set of "true" beacon locations and a rotated scanner,
-- translate the scanner readings so they overlap the known beacons
getTranslated :: KnownBeacons -> Scanner -> Maybe Beacons
getTranslated known scanner = listToMaybe $ do
   let xs = beacons known
   let ys = beacons scanner
   x <- xs
   y <- ys
   let t = x - y
   let ys' = map (t+) ys
   guard (hasLength 12 $ intersect xs ys')
   return ys'

-- Aligns and adds scanner readings to a set of "true" beacon locations
align :: KnownBeacons -> Scanner -> Maybe KnownBeacons
align known scanner = fmap (known <+) $ listToMaybe $ catMaybes $ do
   rotScan <- getRotations scanner
   guard $ hasTranslation known rotScan
   return $ getTranslated known rotScan

(<+) :: KnownBeacons -> Beacons -> KnownBeacons
known <+ new = makeKnownBeacons (beacons known `union` new)

triangulate :: [Scanner] -> KnownBeacons
triangulate (s:ss) = aux s ss where
   aux :: KnownBeacons -> [Scanner] -> KnownBeacons
   aux known [] = known
   aux known (s:c) = case align known s of
      Just k -> aux k c
      Nothing -> aux known (c ++ [s])

makeKnownBeacons :: Beacons -> KnownBeacons
makeKnownBeacons = makeScanner 0

part1 :: [Scanner] -> Int
part1 = length . beacons . triangulate

--------------------------------------------------------------------------------

makeScanner :: Int -> [Vector Int] -> Scanner
makeScanner n vs = Scanner n vs (diffs vs)

sampleInput :: [Scanner]
sampleInput = 
   [ makeScanner 0 [Vec(404)(-588)(-901), Vec(528)(-643)(409), Vec(-838)(591)(734), Vec(390)(-675)(-793), Vec(-537)(-823)(-458), Vec(-485)(-357)(347), Vec(-345)(-311)(381), Vec(-661)(-816)(-575), Vec(-876)(649)(763), Vec(-618)(-824)(-621), Vec(553)(345)(-567), Vec(474)(580)(667), Vec(-447)(-329)(318), Vec(-584)(868)(-557), Vec(544)(-627)(-890), Vec(564)(392)(-477), Vec(455)(729)(728), Vec(-892)(524)(684), Vec(-689)(845)(-530), Vec(423)(-701)(434), Vec(7)(-33)(-71), Vec(630)(319)(-379), Vec(443)(580)(662), Vec(-789)(900)(-551), Vec(459)(-707)(401)]
   , makeScanner 1 [Vec(686)(422)(578), Vec(605)(423)(415), Vec(515)(917)(-361), Vec(-336)(658)(858), Vec(95)(138)(22), Vec(-476)(619)(847), Vec(-340)(-569)(-846), Vec(567)(-361)(727), Vec(-460)(603)(-452), Vec(669)(-402)(600), Vec(729)(430)(532), Vec(-500)(-761)(534), Vec(-322)(571)(750), Vec(-466)(-666)(-811), Vec(-429)(-592)(574), Vec(-355)(545)(-477), Vec(703)(-491)(-529), Vec(-328)(-685)(520), Vec(413)(935)(-424), Vec(-391)(539)(-444), Vec(586)(-435)(557), Vec(-364)(-763)(-893), Vec(807)(-499)(-711), Vec(755)(-354)(-619), Vec(553)(889)(-390)]
   , makeScanner 2 [Vec(649)(640)(665), Vec(682)(-795)(504), Vec(-784)(533)(-524), Vec(-644)(584)(-595), Vec(-588)(-843)(648), Vec(-30)(6)(44), Vec(-674)(560)(763), Vec(500)(723)(-460), Vec(609)(671)(-379), Vec(-555)(-800)(653), Vec(-675)(-892)(-343), Vec(697)(-426)(-610), Vec(578)(704)(681), Vec(493)(664)(-388), Vec(-671)(-858)(530), Vec(-667)(343)(800), Vec(571)(-461)(-707), Vec(-138)(-166)(112), Vec(-889)(563)(-600), Vec(646)(-828)(498), Vec(640)(759)(510), Vec(-630)(509)(768), Vec(-681)(-892)(-333), Vec(673)(-379)(-804), Vec(-742)(-814)(-386), Vec(577)(-820)(562)]
   , makeScanner 3 [Vec(-589)(542)(597), Vec(605)(-692)(669), Vec(-500)(565)(-823), Vec(-660)(373)(557), Vec(-458)(-679)(-417), Vec(-488)(449)(543), Vec(-626)(468)(-788), Vec(338)(-750)(-386), Vec(528)(-832)(-391), Vec(562)(-778)(733), Vec(-938)(-730)(414), Vec(543)(643)(-506), Vec(-524)(371)(-870), Vec(407)(773)(750), Vec(-104)(29)(83), Vec(378)(-903)(-323), Vec(-778)(-728)(485), Vec(426)(699)(580), Vec(-438)(-605)(-362), Vec(-469)(-447)(-387), Vec(509)(732)(623), Vec(647)(635)(-688), Vec(-868)(-804)(481), Vec(614)(-800)(639), Vec(595)(780)(-596)]
   , makeScanner 4 [Vec(727)(592)(562), Vec(-293)(-554)(779), Vec(441)(611)(-461), Vec(-714)(465)(-776), Vec(-743)(427)(-804), Vec(-660)(-479)(-426), Vec(832)(-632)(460), Vec(927)(-485)(-438), Vec(408)(393)(-506), Vec(466)(436)(-512), Vec(110)(16)(151), Vec(-258)(-428)(682), Vec(-393)(719)(612), Vec(-211)(-452)(876), Vec(808)(-476)(-593), Vec(-575)(615)(604), Vec(-485)(667)(467), Vec(-680)(325)(-822), Vec(-627)(-443)(-432), Vec(872)(-547)(-609), Vec(833)(512)(582), Vec(807)(604)(487), Vec(839)(-516)(451), Vec(891)(-625)(532), Vec(-652)(-548)(-490), Vec(30)(-46)(-14)] ]

myInput :: [Scanner]
myInput = []
