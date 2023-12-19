module AdventOfCode.Y2023.Day06 where


type Input = [(Time, Distance)]
type Time = Int
type Distance = Int
type Speed = Int

--------------------------------------------------------------------------------

part1 :: Input -> Int
part1 input = product $ do
   (t,d) <- input
   let opts = options t
   let winningOpts = filter (\opt -> totalDistance opt > d) opts
   pure $ length winningOpts

initialSpeed :: Time -> Speed
initialSpeed = id

totalDistance :: (Time,Time) -> Distance
totalDistance (holdTime,travelTime) = let
   speed = initialSpeed holdTime
   in speed * travelTime

options :: Time -> [(Time,Time)]
options t = [(h, t-h) | h <- [0..t]]

--------------------------------------------------------------------------------

part2 :: Input -> Int
part2 input = let
   raceTime = (read $ concatMap (show . fst) input) :: Int
   raceDist = (read $ concatMap (show . snd) input) :: Int
   t' = fromIntegral raceTime
   d' = fromIntegral raceDist
   -- quadratic formula
   loBound = (t' - sqrt (t' ** 2 - 4 * d')) / 2
   hiBound = (t' + sqrt (t' ** 2 - 4 * d')) / 2
   loTime = maximum $ getWinningHoldTimes loBound (raceTime,raceDist)
   hiTime = minimum $ getWinningHoldTimes hiBound (raceTime,raceDist)
   in hiTime - loTime + 1

getWinningHoldTimes :: Double -> (Time,Distance) -> [Time]
getWinningHoldTimes bound (t,d) = filter (\opt -> totalDistance (opt, t-opt) > d) [floor bound .. ceiling bound]

--------------------------------------------------------------------------------

sampleInput :: Input
sampleInput = [(7,9), (15,40), (30,200)]

myInput :: Input
myInput = [(60,475), (94,2138), (78,1015), (82,1650)]
