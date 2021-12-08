module AdventOfCode.Y2021.Day8 where

import AdventOfCode.Common.List (count, groupOn)
import AdventOfCode.Common.Util (printLines)
import Data.List
import Data.Maybe

--------------------------------------------------------------------------------

zero = "abcefg"   -- 6
one = "cf"        -- 2 UNIQUE
two = "acdeg"     -- 5
three = "acdfg"   -- 5
four = "bcdf"     -- 4 UNIQUE
five = "abdfg"    -- 5
six = "abdefg"    -- 6
seven = "acf"     -- 3 UNIQUE
eight = "abcdefg" -- 7 UNIQUE
nine = "abcdfg"   -- 6

digitSettings :: [String]
digitSettings = [zero, one, two, three, four, five, six, seven, eight, nine]

digitSettingsByLength :: [[String]]
digitSettingsByLength = map (\n -> filter (\d -> length d == n) digitSettings) [0..7]

getDigit :: String -> Maybe Int
getDigit s = let s' = sort s in findIndex (s'==) digitSettings

data SignalPattern = SignalPattern
   { signals :: [String]
   , display :: [String]
   } deriving (Eq, Show)

data Mapping = Mapping
   { input :: String
   , output :: [String]
   } deriving (Eq, Ord, Show)

isCertain :: Mapping -> Bool
isCertain = (1==) . length . output

isSingleChar :: Mapping -> Bool
isSingleChar = (1==) . length . input

--------------------------------------------------------------------------------

part1 :: [SignalPattern] -> Int
part1 = count ((`elem` [2,4,3,7]) . length) . foldMap display

--------------------------------------------------------------------------------

-- Checks if x contains all the characters of y
-- Assumes no duplicate characters
contains :: String -> String -> Bool
contains x y = length (x \\ y) == length x - length y

-- Removes the characters of y from x if x `contains` y
removeStr :: String -> String -> Maybe String
removeStr y x = if x `contains` y then Just (x \\ y) else Nothing

-- (a,b) is a mapping that is known to be true, m is a mapping that may be true, or uncertain
-- If input m `contains` a, then remove a from input m and remove b from all outputs of m
-- If an output of m does not `contain` b, then discard it --- it was an invalid output
trySubstitute :: (String, String) -> Mapping -> Mapping
trySubstitute (a,b) m = case removeStr a $ input m of
   Just x' -> Mapping x' $ catMaybes $ map (removeStr b) $ output m
   Nothing -> m

-- Applies trySubstitute to a list of mappings
-- Make sure not to reduce any mappings to Mapping "" []
trySubstituteAll :: (String, String) -> [Mapping] -> [Mapping]
trySubstituteAll (a,b) ms = let
   (skip, try) = partition (\m -> input m == a) ms
   in skip ++ map (trySubstitute (a,b)) try

-- Resolves contradictory mappings by combining their options
-- Alos keeps the list ordered
fixMappings :: [Mapping] -> [Mapping]
fixMappings mappings = let
   grouped = groupOn input $ sort mappings
   combine ms = Mapping (input $ head ms) $ foldr1 intersect $ map output ms
   in map combine grouped

-- Produces a set of potential mappings from a signal reading
makeMappings :: SignalPattern -> [Mapping]
makeMappings sigpat = let
   allStrings = signals sigpat ++ display sigpat
   makeMapping s = Mapping s (digitSettingsByLength !! length s)
   in fixMappings $ map makeMapping $ filter (\s -> length s < 7) allStrings

-- Takes one step to simplify a set of mappings by substituting certain mappings into uncertain ones
reduce :: [Mapping] -> [Mapping]
reduce mappings = let
   certains = map (\m -> (input m, head $ output m)) $ filter isCertain mappings
   mappings' = foldr trySubstituteAll mappings certains
   in fixMappings mappings'

-- Checks if a mapping cannot be reduced further
isBasis :: [Mapping] -> Bool
isBasis ms = all isCertain ms && all isSingleChar ms

-- Applies the reduce function to a set of mappings until none can be reduced further
-- From this we get a function which unscrambles our wires
-- (Takes an observed signal and produces the segment is connects to)
getBasis :: [Mapping] -> (Char -> Char)
getBasis ms = let
   finalMappings = head $ dropWhile (not . isBasis) $ iterate reduce ms
   list = map (\m -> (head $ input m, head $ head $ output m)) finalMappings
   in \c -> fromJust $ lookup c list

-- Compute the 4-digit number displayed by solving the scrambled signal
outputValue :: SignalPattern -> Int
outputValue sigpat = let
   f = getBasis $ makeMappings sigpat
   digit = fromJust . getDigit . map f
   in read $ concat $ map show $ map digit $ display sigpat

part2 :: [SignalPattern] -> Int
part2 = sum . map outputValue

--------------------------------------------------------------------------------

make :: [String] -> [String] -> SignalPattern
make xs ys = SignalPattern (map sort xs) (map sort ys)

sampleInputSmall :: SignalPattern
sampleInputSmall = make ["acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"] ["cdfeb", "fcadb", "cdfeb", "cdbaf"]

sampleInput :: [SignalPattern]
sampleInput = 
   [ make ["be", "cfbegad", "cbdgef", "fgaecd", "cgeb", "fdcge", "agebfd", "fecdb", "fabcd", "edb"] ["fdgacbe", "cefdb", "cefbgd", "gcbe"]
   , make ["edbfga", "begcd", "cbg", "gc", "gcadebf", "fbgde", "acbgfd", "abcde", "gfcbed", "gfec"] ["fcgedb", "cgb", "dgebacf", "gc"]
   , make ["fgaebd", "cg", "bdaec", "gdafb", "agbcfd", "gdcbef", "bgcad", "gfac", "gcb", "cdgabef"] ["cg", "cg", "fdcagb", "cbg"]
   , make ["fbegcd", "cbd", "adcefb", "dageb", "afcb", "bc", "aefdc", "ecdab", "fgdeca", "fcdbega"] ["efabcd", "cedba", "gadfec", "cb"]
   , make ["aecbfdg", "fbg", "gf", "bafeg", "dbefa", "fcge", "gcbea", "fcaegb", "dgceab", "fcbdga"] ["gecf", "egdcabf", "bgf", "bfgea"]
   , make ["fgeab", "ca", "afcebg", "bdacfeg", "cfaedg", "gcfdb", "baec", "bfadeg", "bafgc", "acf"] ["gebdcfa", "ecba", "ca", "fadegcb"]
   , make ["dbcfg", "fgd", "bdegcaf", "fgec", "aegbdf", "ecdfab", "fbedc", "dacgb", "gdcebf", "gf"] ["cefg", "dcbef", "fcge", "gbcadfe"]
   , make ["bdfegc", "cbegaf", "gecbf", "dfcage", "bdacg", "ed", "bedf", "ced", "adcbefg", "gebcd"] ["ed", "bcgafe", "cdgba", "cbgef"]
   , make ["egadfb", "cdbfeg", "cegd", "fecab", "cgb", "gbdefca", "cg", "fgcdab", "egfdb", "bfceg"] ["gbdfcae", "bgc", "cg", "cgb"]
   , make ["gcafb", "gcf", "dcaebfg", "ecagb", "gf", "abcdeg", "gaef", "cafbge", "fdbac", "fegbdc"] ["fgae", "cfgab", "fg", "bagce"] ]

myInput :: [SignalPattern]
myInput =
   [ make ["dbcfeag", "cgaed", "fe", "bfgad", "aefcdb", "efa", "efgda", "gcef", "dcaebg", "dfeagc"] ["fae", "cfge", "fae", "baefdc"]
   , make ["ea", "bfecadg", "bgafcd", "deac", "ebcag", "eab", "debfag", "eabgdc", "bcgfe", "bagdc"] ["cdbagf", "fagbcd", "bae", "fcegb"]
   , make ["fbgdec", "cbgeaf", "cbfag", "bd", "bgda", "cgbfda", "dbf", "fecda", "bfadc", "cbedagf"] ["gecfdb", "cbfga", "bacgef", "dfb"]
   , make ["geabdf", "cfbge", "dcbeaf", "aebdf", "dgea", "acdgfbe", "fag", "gfbea", "ag", "cabdfg"] ["dfeabc", "dbaefc", "fdaebc", "fga"]
   , make ["gcae", "cefdg", "abdcgfe", "cg", "fcg", "afcegd", "dbgfea", "agdfe", "dcafbg", "bcdef"] ["gc", "caefdg", "gcf", "egdcf"]
   , make ["efgcb", "fbac", "dbefg", "bce", "efacg", "fbgcdae", "cb", "egfbca", "dgfaec", "dcgbea"] ["fgecb", "bcfa", "bc", "efcag"]
   , make ["efcgdb", "da", "bdefg", "dfegba", "fbacg", "gbcdea", "dbafceg", "adb", "feda", "fadgb"] ["da", "ad", "cgabde", "cebdga"]
   , make ["ca", "fcabegd", "facd", "agbed", "acg", "cfadeg", "acdeg", "agcefb", "cdfgeb", "efcdg"] ["cdeafg", "cag", "efgadc", "gacbfe"]
   , make ["fdacgb", "cfa", "cfage", "gabce", "dfagce", "edagfb", "dcbfeag", "fc", "fced", "gfdae"] ["gabdef", "fdegba", "gaedcf", "fac"]
   , make ["acgdbf", "faedcbg", "fa", "cbgdae", "fgbdae", "aedbg", "adf", "fbea", "cfdge", "fedga"] ["fad", "egdcf", "feab", "bfcagd"]
   , make ["ged", "cadgf", "gcdafeb", "cabgfd", "cgedf", "aedc", "ebfcg", "faebgd", "ed", "fcadeg"] ["dfgca", "gabefd", "dafgc", "ecfgd"]
   , make ["bcedga", "dceba", "gcadfbe", "dfec", "adcbfe", "fda", "fabge", "df", "fbdae", "bcfgda"] ["adgcfbe", "fbgae", "dgebafc", "gacedb"]
   , make ["fgcabde", "edbcfg", "efcdga", "dfbcea", "dbaeg", "fea", "fa", "dbaef", "acfb", "dbfec"] ["aecbfd", "aedcfb", "bdaef", "abfc"]
   , make ["cfbd", "gefad", "dce", "fdeac", "dc", "ecabfd", "dgabce", "gabdecf", "afbce", "fagceb"] ["bdfc", "bcgaed", "bgdace", "afegd"]
   , make ["acfgde", "egdfb", "dcaf", "gecfd", "ecdbgfa", "gcdbae", "cefgba", "dcg", "gcefa", "cd"] ["eadcbg", "cdfega", "fdgeac", "dgc"]
   , make ["cadfgb", "edcgbfa", "dc", "cgd", "fegcb", "dfabg", "cdaebg", "dafc", "fcbdg", "gbdeaf"] ["afdc", "adcf", "befdag", "gfcdb"]
   , make ["deafg", "ebafdcg", "gcdfba", "bge", "eb", "fgcbd", "gbefac", "bdce", "fdebg", "gdcfeb"] ["cbdagf", "bdce", "gdfeb", "fgead"]
   , make ["acbdef", "bg", "agebdf", "afbcg", "fgbeca", "edafcgb", "eacfb", "gba", "ebgc", "fgcad"] ["begdaf", "gcbe", "ebdfag", "dabefg"]
   , make ["ga", "agc", "bdegcaf", "gefbc", "cfdea", "dafgec", "fagce", "dgebca", "aefbdc", "gdfa"] ["acfedbg", "bfaecdg", "gca", "facedg"]
   , make ["abdg", "ad", "egcabf", "ebagf", "afd", "gdfceab", "dcebf", "dagbef", "cdgafe", "deabf"] ["bfedc", "cbedf", "gdab", "afd"]
   , make ["efbadc", "cafbd", "eba", "fegda", "fcbe", "dbefa", "efcbdag", "adbcge", "badfcg", "be"] ["be", "agdfe", "cedbag", "dcbeaf"]
   , make ["adegb", "cgafb", "badgcfe", "fged", "fgaebd", "eabgf", "bceafd", "eaf", "eacbdg", "fe"] ["bdgeac", "adgbfe", "bdgae", "acfgbed"]
   , make ["dbfega", "befag", "gbfdec", "gacfed", "fcgdabe", "bgf", "deagf", "bf", "dafb", "bceag"] ["bdfa", "cagbe", "gceba", "agbdfe"]
   , make ["fecagd", "cbfeagd", "fdecb", "fg", "fgbde", "ebgda", "cagdbe", "fge", "gfab", "agefdb"] ["geabcd", "gdbea", "ebgafd", "egdab"]
   , make ["ab", "agdcfe", "dcfgb", "cba", "adefcb", "adfbc", "bdae", "ebacgf", "aegbdcf", "fcade"] ["gfecda", "dfabc", "edab", "bfcad"]
   , make ["bcfae", "ebd", "gefdacb", "fdceg", "dbaegc", "gdbf", "gcefbd", "cedfb", "adfgce", "bd"] ["gebdcf", "bd", "dgfb", "ebdcgf"]
   , make ["gbcafde", "cdaef", "dfe", "ecbf", "egcda", "fadgcb", "bfdac", "dacfbe", "bgefda", "ef"] ["dcgafb", "dfaebg", "fbec", "agbdfc"]
   , make ["fe", "cfedab", "dcfgeb", "gdbef", "gadeb", "cegf", "fgcbd", "fbe", "abdcgf", "ebacgfd"] ["debfg", "abecfd", "adbefc", "gdcbfe"]
   , make ["ad", "cbfeda", "acegfb", "gdcaeb", "adcgfbe", "bda", "abefc", "dcabf", "fgcdb", "afde"] ["gdcbf", "cedbaf", "dbagec", "becfa"]
   , make ["acegb", "efcgab", "edcfgab", "aebdc", "gce", "ge", "fage", "egcdfb", "agfbc", "cbdfga"] ["eg", "cafegb", "fcbeag", "acbgef"]
   , make ["bcaf", "agefdc", "bcegaf", "eaf", "bdgfe", "agfdecb", "efgab", "af", "abegc", "dgacbe"] ["defbg", "dgaebc", "eaf", "cebga"]
   , make ["edabcf", "bdeg", "gd", "cadebg", "dgcab", "gfcaed", "febdgca", "cdg", "beacd", "cagbf"] ["befadcg", "fbgca", "afdecg", "bgceda"]
   , make ["gadfeb", "ag", "gfda", "cbeda", "fbeacg", "bgdcaef", "dgfeb", "eag", "gbdea", "fegcbd"] ["cabefg", "degbcf", "gdbef", "bcdgef"]
   , make ["efbcda", "cgfd", "dgefac", "dfcgbae", "degaf", "adg", "debacg", "aedcf", "gd", "aebgf"] ["ecfad", "cgbade", "aegdcb", "gfbae"]
   , make ["bdc", "cdfgb", "cabgf", "gabd", "fdebagc", "eabgcf", "bd", "abdcef", "gfcabd", "fgedc"] ["faecdb", "fgecd", "cbfdag", "bcfadg"]
   , make ["dfagbec", "ceg", "egfad", "fdeabg", "acedg", "afedcg", "gc", "bcgaef", "eacdb", "fdcg"] ["gcfd", "eadbc", "dbegcfa", "eabcd"]
   , make ["gfb", "egfba", "eadcfb", "gf", "adfg", "dgebfa", "dacfegb", "eagcb", "cbedgf", "fadeb"] ["edfba", "afgebd", "adbegf", "efcbda"]
   , make ["fec", "caegd", "efgb", "cgaedfb", "gfbdec", "gfdbc", "ef", "cfdgab", "fdcge", "fbaedc"] ["abcdef", "gcfbd", "bfegdac", "gcead"]
   , make ["gefdba", "abcef", "ead", "gedcab", "ad", "bfdecg", "cbged", "ebcad", "cafbdeg", "cadg"] ["defcgb", "ade", "bgecd", "ceabf"]
   , make ["cabg", "adgefbc", "cb", "ebafg", "febgc", "fbc", "gdcfe", "cafedb", "egfabc", "aebdgf"] ["bcdafeg", "ecbafd", "adbecf", "fbcge"]
   , make ["acged", "febgd", "agdcfb", "bdceg", "gdbecf", "bc", "bgc", "cfeb", "cfgbaed", "bgefad"] ["dceag", "efbc", "cbfdeg", "ebcdfg"]
   , make ["gab", "adgcbef", "cebfg", "abdce", "dcbefg", "bafdgc", "abefgc", "aegf", "ag", "cgbea"] ["gaceb", "aegf", "fgcbe", "cgebf"]
   , make ["fa", "baf", "cadfbge", "dbgef", "ebacgd", "dfceba", "adbec", "fdeba", "cfae", "dacgfb"] ["dabcef", "dafcbe", "acgdbe", "ecdfab"]
   , make ["fgeca", "gecadfb", "aecdfb", "fb", "gcbed", "dfgb", "cgdbea", "ecfgb", "gfbdec", "efb"] ["abcedg", "bf", "bf", "agedcfb"]
   , make ["fg", "aefgd", "bafcde", "gcfe", "bdgae", "gfa", "fedca", "cagdfb", "cfaedg", "ebgacfd"] ["dcfgab", "dgfea", "gf", "cafdgb"]
   , make ["afbcge", "debgc", "cafdbg", "ae", "eadf", "afdgb", "gbdeaf", "aeb", "cgfebad", "abgde"] ["deaf", "cbafge", "cbegd", "aedbg"]
   , make ["fbc", "afcegb", "fadgc", "fbeda", "cbed", "ecdafb", "cfdab", "cb", "fegdcab", "bdaefg"] ["cgfad", "ecbd", "edafb", "cgafd"]
   , make ["fegbcd", "abcf", "gfabedc", "bgcad", "gfacd", "gedab", "efdagc", "dfgcab", "cdb", "bc"] ["fbca", "cdbag", "cdfgab", "cgedbfa"]
   , make ["gfca", "gbdef", "eadcfg", "ebdcag", "dcg", "afecd", "gc", "decafbg", "fcegd", "bcfade"] ["gcefd", "bcdafge", "cdefg", "edfgc"]
   , make ["gdae", "ad", "gecfd", "dcfaeb", "dfegcb", "gdcaf", "fdacge", "adf", "agbcf", "cebfgad"] ["dgebafc", "edfgc", "da", "cbgafed"]
   , make ["ceg", "febgdca", "bfgdea", "gc", "adcfge", "cefbag", "agdc", "edfcb", "dfgce", "gadef"] ["ceg", "ebdagf", "agdfe", "afbgce"]
   , make ["dabegcf", "bcdfag", "gdecb", "gdbeca", "gaebd", "fbdce", "bdagef", "cg", "bgc", "ecag"] ["dfcabg", "abfdge", "gadbe", "cg"]
   , make ["dacefb", "efc", "gfaebc", "gbfe", "egcda", "agfbdc", "beadfcg", "fe", "fbcga", "cgafe"] ["ef", "fbaedc", "ef", "efc"]
   , make ["cbgae", "faedbg", "fegab", "agc", "ebdgc", "eacf", "bgeafc", "ca", "cgdbafe", "gadcbf"] ["eagfb", "aebgf", "gcbfad", "gfeba"]
   , make ["dcf", "dacbg", "bdcaef", "dfeacg", "gacfe", "cdfag", "fgde", "faecgb", "daebcgf", "df"] ["fcd", "fecadb", "egacf", "deabcf"]
   , make ["egdfa", "dfaec", "gdab", "ga", "agf", "dbefga", "dgebf", "afbgecd", "bgafec", "bgefcd"] ["fegbd", "gdfbe", "begdfc", "efcbadg"]
   , make ["cfd", "edafbc", "afbd", "cadbeg", "fgecbad", "gcaef", "dacef", "df", "dbcae", "efcdgb"] ["adbf", "cabde", "baegdc", "cgbeda"]
   , make ["cbag", "gc", "cdgbe", "defbg", "agebdfc", "becda", "dbgcae", "cdg", "efacbd", "fecgad"] ["fcegabd", "abgc", "dacbe", "cdaebg"]
   , make ["bf", "dacgf", "dfb", "befc", "fbedcg", "gebdc", "dbacefg", "abdegf", "gcdfb", "bedacg"] ["bfec", "feagcbd", "egdcb", "daegbf"]
   , make ["aebgfdc", "gadcf", "gcdfeb", "dagce", "agdeb", "acfe", "bgacdf", "dec", "gfecad", "ec"] ["ceadg", "edcag", "fdcga", "cdbagef"]
   , make ["befdgca", "dgcbf", "fedagc", "be", "bfe", "dcefb", "ecfdba", "acfed", "ebca", "ebgfda"] ["dfbcae", "bafgdce", "bgcdf", "egbdfa"]
   , make ["bfa", "bdagfc", "dbega", "bfgea", "eagfdb", "dbgace", "bcefg", "dcbgeaf", "fa", "defa"] ["ebcgdaf", "aecgdbf", "fgcadb", "fa"]
   , make ["agcdeb", "cgf", "agbdcef", "bgdfe", "efac", "cgefb", "abefgc", "cagdbf", "fc", "gebca"] ["fegbc", "acfe", "becga", "deagbc"]
   , make ["adbfce", "agebd", "eadcbfg", "ega", "abedf", "fgab", "dgebc", "dgfcea", "ag", "agdebf"] ["gdfcabe", "ebadf", "ga", "bfag"]
   , make ["gcebfd", "feacdbg", "acegfb", "afgce", "gbac", "geb", "cdaefg", "daefb", "gb", "fgbae"] ["agefb", "gb", "adfbe", "efabd"]
   , make ["gadbfc", "ebgad", "dgecba", "fgae", "eacgdbf", "af", "ebcfd", "afd", "bdeaf", "gadefb"] ["fa", "bdgaef", "gdbacf", "af"]
   , make ["bfeacd", "edgcabf", "dg", "gcabd", "cdaeb", "acfgb", "bcgdae", "dgae", "cgd", "bgfdec"] ["bedac", "acbed", "abcdfge", "dgc"]
   , make ["cdage", "dgabec", "cbadg", "ge", "acfde", "edg", "becfagd", "gbec", "adbcgf", "fbeadg"] ["cdbgea", "cgdaeb", "cegad", "gdeac"]
   , make ["ab", "gbaf", "cdegba", "fceadbg", "gfbecd", "bca", "eadcf", "fgeabc", "becfa", "cebfg"] ["cagfeb", "cefabg", "fcbeg", "eadcgb"]
   , make ["afdec", "bcaefg", "ca", "abcd", "efcdg", "aec", "aebdf", "cdafbe", "begdaf", "eadgbfc"] ["cdfeg", "afdce", "edfac", "gdfabe"]
   , make ["faebd", "fed", "df", "bgdcaef", "cbdega", "gdbeaf", "fbecgd", "badge", "afebc", "fadg"] ["dcebfg", "dgaf", "fd", "cdegfba"]
   , make ["abdgf", "efg", "fe", "gdeabf", "cdbge", "cfadeg", "gdbef", "cafgdeb", "fbea", "bgcadf"] ["bgdfe", "fabe", "gbfadc", "gbfcda"]
   , make ["agdfe", "cfbd", "dgfca", "cd", "abgfc", "cdgbea", "gbecfa", "gbadcef", "cda", "dfgbac"] ["cegfba", "fbcd", "bfdc", "dca"]
   , make ["acdb", "bdcega", "dbfcge", "aedfgb", "dec", "fcega", "dc", "adegc", "fgceabd", "gaedb"] ["badeg", "bdecgf", "cd", "cdageb"]
   , make ["abe", "ab", "dbecfag", "adgec", "agbf", "eadbg", "cbeafd", "fbcegd", "gadbfe", "bfgde"] ["ebgfd", "efdgb", "gceadbf", "ba"]
   , make ["dgfceba", "egbcaf", "fgc", "bfgdca", "cbgfe", "cg", "faebc", "agce", "gdfeb", "debcaf"] ["bedfac", "edbfg", "aegc", "eabcdgf"]
   , make ["bdfac", "dcfeb", "dbcefa", "dbgcaef", "fbae", "egdcb", "fgaecd", "fec", "cdbafg", "ef"] ["bdfagc", "dcfabg", "gdbfeca", "eabf"]
   , make ["gfca", "dbcaf", "bfcgd", "fcgdeab", "degcb", "abcedf", "cbagfd", "gfb", "fg", "fdgabe"] ["gcaf", "bfg", "fbg", "gf"]
   , make ["efcagd", "dbag", "fgd", "defgacb", "aebfg", "dbgfe", "bdeafg", "edfbc", "gd", "fgecab"] ["fbeag", "ecgfabd", "degacf", "debgfac"]
   , make ["efcbg", "bacfe", "gbdce", "fge", "acegbdf", "cgdf", "aebdfg", "dgbcae", "ebcdgf", "gf"] ["caefb", "cbfdage", "ecfgb", "bgedc"]
   , make ["fgdeb", "dcbfg", "fgae", "fbeagd", "edbfa", "aegdbc", "egb", "gafecdb", "bfdeca", "eg"] ["dgbcea", "bdaef", "gfbde", "dfgbea"]
   , make ["bdcaf", "gdafc", "cbf", "bdefa", "cb", "fgceda", "bdfagc", "fcbgde", "afcgedb", "cbga"] ["gbca", "fbdca", "cegfdb", "fcagd"]
   , make ["cfebgd", "cedgfa", "dcfabge", "dbfgc", "dgeb", "ge", "fcegb", "gfe", "abfdcg", "fecab"] ["fge", "fabec", "gfbadec", "bfceg"]
   , make ["aeb", "ab", "gebfd", "abcdef", "edcag", "cdegaf", "gaedbc", "bgfecda", "debag", "gbac"] ["beacgd", "bdfeg", "afgecd", "gedbf"]
   , make ["ab", "gebfadc", "acfge", "edafbc", "abegc", "dcegab", "gbecfd", "gbda", "cab", "ebcdg"] ["dagb", "cegfa", "bca", "bac"]
   , make ["gaedb", "ebcd", "fgcea", "cba", "dcgafb", "febadg", "egcab", "adcbge", "efgadcb", "cb"] ["dfcagb", "cbed", "bagce", "dbce"]
   , make ["ca", "dgfceb", "bfeacd", "fcbgd", "bdgca", "cfag", "gaedb", "abc", "fcbagd", "fgecadb"] ["cgdfab", "cfag", "bac", "cbfegad"]
   , make ["gade", "gafcb", "bfgecd", "bge", "ecbda", "gdcbea", "eg", "beacg", "fdaecb", "agfbecd"] ["dgae", "bgfca", "abegc", "cfdebg"]
   , make ["eac", "cdbef", "ac", "gfdae", "dagc", "afcedgb", "dcfae", "dafcge", "dbfega", "caebfg"] ["gafceb", "dbeacgf", "cdga", "fbcde"]
   , make ["dbagfc", "bgedca", "df", "afgec", "daf", "beagdfc", "aefcbd", "bedac", "acfed", "dbfe"] ["bedca", "cdaef", "baecfd", "befcad"]
   , make ["fdacb", "baedc", "acedfb", "bacedg", "fd", "gfcab", "ecdf", "daf", "fedgba", "befgdac"] ["egabcd", "cedab", "eabdfg", "adf"]
   , make ["efdgc", "dfecga", "adfbe", "ac", "cage", "adc", "gfbdec", "cagbfd", "efdabgc", "caefd"] ["efdab", "efdgcab", "ca", "cedfg"]
   , make ["ca", "acgf", "cadbgfe", "egfcab", "gefab", "cea", "defbc", "fecab", "edbcga", "fbgdae"] ["cfeba", "efdcbag", "dacebg", "agefcdb"]
   , make ["bcedfa", "agecd", "ebgf", "fcbdg", "fde", "cgfed", "acgbfd", "cagfdeb", "ef", "cgbdef"] ["fgeb", "bcfdg", "def", "ef"]
   , make ["bfcdea", "ga", "fbdegac", "gecdf", "cfdab", "bcgadf", "facdg", "aegbcd", "gac", "afgb"] ["cbadfg", "facdb", "afbg", "dfecba"]
   , make ["fdcae", "afeg", "fa", "bceda", "dfebcga", "afcgdb", "dgecf", "acf", "fgceda", "cbfdeg"] ["fa", "bdfcga", "fcbegd", "efacd"]
   , make ["efgbcd", "cf", "bdgace", "fgbad", "dfc", "fceabd", "dbaegfc", "bdcgf", "becdg", "gfce"] ["bcedaf", "adbegc", "badgec", "bgcfd"]
   , make ["efcba", "ebdfga", "fda", "gdefbc", "da", "facde", "dacg", "fgedc", "gfeabcd", "acdefg"] ["da", "bgafde", "cegfd", "edafgb"]
   , make ["daegb", "cbge", "bgcaed", "afgced", "ge", "cdabe", "fdbcaeg", "fcbeda", "bgfda", "eag"] ["gae", "egcb", "egbc", "egdafcb"]
   , make ["cefda", "fadg", "bfedac", "bedfacg", "dgecfa", "eag", "ga", "fegcb", "becagd", "gcefa"] ["dcfea", "eag", "cedfa", "bgfcaed"]
   , make ["dcfe", "gbeafd", "df", "bdcaf", "ebfcag", "fcedagb", "cdbag", "dcfaeb", "eabcf", "fbd"] ["dfce", "geacbf", "cefd", "ebdacf"]
   , make ["efgbc", "agbd", "gd", "eadgcf", "becdfa", "bdace", "dge", "debgc", "adecgb", "bfdgcae"] ["bgcfe", "bedcg", "abgd", "gdba"]
   , make ["cdbf", "edbfgc", "feadg", "cd", "ced", "gaecdb", "degfc", "efcgb", "fcbgaed", "befcga"] ["cfabgde", "fgdcbe", "efacgb", "abgcef"]
   , make ["ae", "caegfd", "eadgc", "fedgba", "fecgd", "gbacd", "efac", "aed", "afcbged", "cgdbef"] ["ae", "eacf", "abdegcf", "eda"]
   , make ["fc", "afcge", "dbgecf", "adcgfe", "eafbg", "gdebacf", "eagcd", "gfc", "aecdgb", "afcd"] ["cfg", "dgeac", "afbge", "edgcba"]
   , make ["aedcg", "gfd", "cafgebd", "fg", "eadbgc", "adfcb", "cefg", "gabefd", "dacfg", "efcgda"] ["dgeca", "afgedb", "cfdga", "aefgcd"]
   , make ["bfgaed", "facedb", "fbec", "dbgca", "cagfbde", "egcfda", "efdac", "acbfd", "fbd", "fb"] ["efbc", "agcdb", "fagdbe", "bacdf"]
   , make ["fac", "dcae", "fgbda", "fagbce", "bdfac", "cbdfe", "ca", "cadbfe", "aegcdfb", "cebdfg"] ["cdea", "cfbdeg", "ac", "afdcb"]
   , make ["geabcfd", "gfcaeb", "gcbead", "eba", "facbg", "fdgcab", "ea", "bgafe", "fdgbe", "caef"] ["bagef", "cafgb", "afbeg", "cbgaf"]
   , make ["gcfdeb", "ceg", "fabcge", "edbag", "fcabdg", "bcgaf", "ceaf", "ec", "agefbdc", "cbega"] ["bgafc", "baecgdf", "bcfged", "gbdfeac"]
   , make ["cf", "adgef", "cfgbaed", "bfdc", "gfc", "egcdfb", "dcgaeb", "degbc", "fcedg", "ecagfb"] ["fdbc", "cgdbe", "gdabce", "gfdce"]
   , make ["bc", "afgcb", "cfdga", "dfcb", "cbdgea", "bgc", "efcgdab", "cfedga", "beafg", "bagcfd"] ["bc", "bc", "cbfadg", "adcbge"]
   , make ["egdfb", "fa", "agfbe", "eacf", "befagc", "fba", "fbedacg", "cadebg", "agcbe", "cfgbda"] ["bagfec", "bcdgaf", "cbfgad", "dgcbfa"]
   , make ["acgbde", "acgdbef", "cba", "ca", "bcgfd", "cade", "egdafb", "aebgfc", "agbed", "adgcb"] ["fdgbc", "gbadef", "gfcdbae", "edfgab"]
   , make ["egbac", "efacgd", "gfbd", "fbcdag", "fecbad", "dgc", "badcf", "gfcebda", "dbcag", "dg"] ["bacdg", "facbegd", "cgd", "dfcgae"]
   , make ["adgcf", "fbgcda", "efcgda", "dcbea", "fedg", "cfebag", "fae", "ef", "acdef", "gecbfda"] ["fcagd", "gcadf", "gcefdba", "gefabc"]
   , make ["fegdba", "ba", "cedgaf", "gbade", "bfad", "egfda", "gba", "cadbgef", "gbced", "fcaegb"] ["gedaf", "bdega", "abgde", "bafd"]
   , make ["gbacef", "gdab", "defgc", "bagfdc", "dfb", "afgcb", "efdbac", "bd", "dfbcg", "befcgad"] ["cdgef", "bd", "gbfac", "ecbgfad"]
   , make ["fgacd", "ef", "dagfce", "ecdfa", "fae", "adfbgc", "cegf", "febdga", "ceabd", "bgacedf"] ["abcgfed", "gfce", "fae", "efacgdb"]
   , make ["fbgdc", "afedg", "gcbafd", "gfedbc", "cfdag", "acd", "fbdeacg", "acbfde", "cgab", "ac"] ["gdcbf", "cedfbg", "dacgf", "dcafg"]
   , make ["becf", "feadb", "abfedc", "dfcageb", "gcbda", "bafedg", "aec", "daecfg", "acdbe", "ec"] ["abdef", "fdeacg", "ace", "ecbf"]
   , make ["dc", "cbfegda", "bfeadg", "dbcg", "gabfd", "adc", "dbcfga", "bface", "fgcade", "fdabc"] ["dacbgf", "dc", "cdfba", "dc"]
   , make ["bc", "bfgedc", "fbc", "adfbceg", "cegb", "degcf", "eabdf", "defcb", "cdfgae", "fcagbd"] ["bcefd", "gceb", "fcagde", "cbfgad"]
   , make ["ge", "cabgd", "egca", "egd", "adgbecf", "bfdagc", "decbag", "agbde", "deafb", "ebdgfc"] ["cagdfb", "bgecda", "cgedba", "fabdcg"]
   , make ["cg", "cfagbd", "gcedfa", "efdbc", "gcd", "gbefda", "fegad", "efdcg", "caegdbf", "agce"] ["fgced", "dgfacb", "befdagc", "cefgda"]
   , make ["bfedgc", "bec", "ce", "fcaebd", "efabg", "gdec", "cgfdb", "aebdfcg", "bfgcda", "cgbfe"] ["agebf", "bgdcfe", "ec", "agcbdf"]
   , make ["db", "dfcbge", "edgfca", "gabd", "dcb", "agfcd", "bafcd", "afbgcd", "ebafc", "edgfbca"] ["acfdg", "cbdfa", "afceb", "gbda"]
   , make ["cadegbf", "egdfbc", "adc", "acbdg", "dgbcf", "adgfbc", "da", "cgfdae", "fbda", "ebcga"] ["dgacb", "bagdc", "fgcbd", "aegbc"]
   , make ["geacf", "abc", "gfedba", "cbadfg", "dbcg", "gfdba", "cagfb", "cb", "bfdcae", "adfecbg"] ["cb", "eadfgb", "bcfag", "dgcb"]
   , make ["abdfcge", "adbeg", "badcef", "egcf", "fg", "fgb", "fbdec", "gebfd", "abgfcd", "gfebdc"] ["cbeadgf", "cfdbag", "gfdbe", "gf"]
   , make ["facdbe", "acgfbd", "dbc", "bd", "abgcd", "adceg", "bcegfa", "fdgb", "bfgca", "badcfeg"] ["ceafdb", "bfcag", "dcgba", "bgdf"]
   , make ["gcda", "cadbe", "abcedf", "cabeg", "edbgcf", "begcda", "begaf", "cg", "acbdefg", "gbc"] ["dbace", "gdbeac", "dacfgbe", "abdce"]
   , make ["cgf", "gf", "fadegbc", "afgcde", "abgced", "fecba", "ceagf", "dcagbf", "fgde", "cdaeg"] ["gdebacf", "gf", "gdfe", "cfega"]
   , make ["abdef", "bc", "gecaf", "dbcgfae", "cbfg", "efcba", "bac", "acebfg", "degcba", "dcegfa"] ["bcfadeg", "afceb", "edfgac", "gcabed"]
   , make ["bfcged", "bgdac", "becad", "fagc", "aegbdfc", "gacfdb", "dbgfc", "dafgbe", "ga", "bga"] ["fgdecab", "abdcg", "cefabdg", "egdbfa"]
   , make ["abcfg", "baecf", "agec", "fcgaeb", "agedbf", "ag", "dacfbeg", "bag", "eafcdb", "fcgdb"] ["bcdfea", "efdabg", "ga", "fdbgc"]
   , make ["ba", "baf", "dfbgce", "cgab", "cefgdab", "efcad", "efacbg", "cgfeb", "ebafgd", "cafeb"] ["fdcegb", "abf", "baf", "ecabf"]
   , make ["cegafd", "egdcba", "gbedf", "cgfde", "cfad", "ecf", "fcegadb", "eagdc", "bgaefc", "cf"] ["bfged", "defgb", "gbfed", "efc"]
   , make ["gedfa", "abdce", "fadbe", "ebfc", "fba", "badgce", "bf", "gabefcd", "fdbgac", "befcad"] ["bcdgaf", "fdbgca", "bfa", "fgbdca"]
   , make ["bacef", "cg", "dgabec", "agfc", "edfgb", "cge", "agfdbec", "fcabeg", "cgebf", "abfdce"] ["decfbga", "cagf", "adcgbe", "dgefb"]
   , make ["eafgd", "ce", "fbgaced", "ecfga", "eafdgc", "gdce", "cea", "cefdba", "cbfag", "bdgafe"] ["eadfbg", "dgec", "efgca", "degfa"]
   , make ["decgabf", "fecd", "acd", "acbegf", "agdec", "gabed", "gacbfd", "faceg", "cd", "fgeadc"] ["dc", "dcafge", "abecgf", "dc"]
   , make ["egd", "fdaebg", "gcbeadf", "fcdge", "ge", "adecf", "gbce", "dgfbac", "dcfgeb", "gdbcf"] ["fdcae", "gfdbea", "bdfcg", "eg"]
   , make ["ad", "cbafe", "dabe", "dfa", "gbfcea", "ceafd", "dcbagf", "fabdce", "bfceadg", "dfgec"] ["cebaf", "aecbgdf", "cgdbfa", "fbaec"]
   , make ["fc", "cdgbae", "gfcbad", "badcg", "fcdba", "feadb", "fac", "cgefad", "fdgbeac", "gfbc"] ["cf", "cfabd", "caf", "gdfcea"]
   , make ["fdgace", "edac", "cdfag", "de", "dcfge", "facebgd", "cgebf", "dfebag", "def", "bafgdc"] ["fdgac", "gacfbed", "edgacf", "dgcfa"]
   , make ["gedfac", "acedb", "fbcadge", "acgefb", "dbgf", "fdaeb", "dgafe", "fgaedb", "bf", "fba"] ["fb", "bf", "bf", "agefcd"]
   , make ["gefbdca", "fagbed", "cgaeb", "gf", "ebdfa", "gdbf", "afebg", "feg", "feadcb", "geacfd"] ["egf", "fedab", "gaefb", "eagbf"]
   , make ["efadgbc", "bfag", "edgcb", "cga", "cefab", "afbegc", "ga", "gaebc", "eafcdb", "daefgc"] ["ga", "acg", "gcfbade", "dgecb"]
   , make ["efcgbd", "gebcf", "gcadbf", "agfce", "fcdeagb", "gfa", "ageb", "aefdc", "ga", "gacbef"] ["gcebaf", "geba", "acfde", "fga"]
   , make ["ga", "adbg", "fdeac", "gfa", "fdgbcae", "dgfabc", "cfgdb", "fegbdc", "fcabge", "agcdf"] ["adbcfg", "efadc", "agcdf", "cgdbf"]
   , make ["febdc", "dacfeb", "gbfce", "cfgae", "bge", "bgdf", "dgcfbea", "baecdg", "gb", "ecfgbd"] ["gb", "fcedb", "becdf", "gb"]
   , make ["bgacfd", "cdfabeg", "adgce", "bcegd", "acdefg", "gb", "cgb", "bgae", "ecbadg", "bdfec"] ["adcbfg", "gdfcea", "deafbcg", "ceadfg"]
   , make ["bc", "gdfeac", "fbaec", "egfcba", "dacbeg", "cab", "gacbfde", "eabfd", "faceg", "fgcb"] ["afbegc", "cdgabe", "bagecf", "ebcdga"]
   , make ["fd", "dfe", "fdgae", "abegfd", "dbcaeg", "dfgb", "caedbf", "efcag", "dageb", "dbfeagc"] ["dgfb", "gaedbf", "gfbd", "dgfcabe"]
   , make ["agefbc", "aefdb", "abdge", "cdef", "bfdcga", "dacbf", "fe", "gbdcefa", "eabcdf", "efb"] ["cdbefa", "adfcbg", "fecagdb", "ebf"]
   , make ["gbade", "adecfg", "abegcdf", "dcbfa", "cgbafd", "cfeb", "dfaebc", "fe", "feabd", "efa"] ["dbcfea", "bcfe", "fecb", "bafde"]
   , make ["dfba", "dfc", "efbac", "cgead", "ecfgab", "gdfecb", "edacbf", "df", "afced", "bgecfad"] ["afdce", "bcfegd", "efgbac", "adgce"]
   , make ["abfcg", "egacf", "bceafg", "dabgc", "fgb", "fb", "fbecadg", "fbce", "gafcde", "gfdbea"] ["abdgc", "defagb", "cbef", "egcaf"]
   , make ["caegb", "decgbfa", "cebf", "edacg", "cgfbae", "gbc", "gfbea", "baedgf", "bc", "bfcdag"] ["agebc", "bgcae", "cbg", "gefba"]
   , make ["ad", "gadf", "feacg", "aed", "agbfce", "egcda", "fedcba", "abcdgef", "bcdge", "gecafd"] ["gedfca", "ad", "febgac", "dcaebf"]
   , make ["cegbd", "bgcfda", "dbfeag", "acef", "gcead", "cagfde", "dcfga", "gae", "fagbced", "ae"] ["defgab", "dacgf", "cebdg", "gecdfa"]
   , make ["fag", "edfcg", "cdfa", "fa", "ceagfd", "fdgabe", "gebac", "egbfcd", "egdcfab", "gcaef"] ["acebg", "fag", "fgdec", "dfegcb"]
   , make ["abe", "bcdfa", "acdbgf", "befd", "eadbc", "acbfgde", "eb", "fgcaeb", "gacde", "fcdbea"] ["eadbc", "afdgbc", "bgaefc", "cabfdeg"]
   , make ["afdgce", "dgabe", "dgfbe", "bdegfc", "dfg", "agcbdfe", "fd", "abegfc", "dcfb", "bfgce"] ["debag", "bedgf", "ebdcfg", "cgebafd"]
   , make ["dgfcea", "eagcbd", "gfceb", "adgb", "cgdeb", "dg", "eacdbf", "bedcafg", "dge", "abedc"] ["ecbgf", "dg", "dbfcgea", "ebcad"]
   , make ["dgecf", "badfce", "egcdfab", "efabd", "edfca", "ac", "badc", "gbcfae", "eagdfb", "caf"] ["feadc", "febad", "dacef", "caf"]
   , make ["gfb", "bacgde", "acbdgf", "afbd", "bdgca", "bf", "bgfdcea", "ecfag", "dgfecb", "fbcag"] ["fbgdcae", "gdfbca", "fbg", "fgaec"]
   , make ["bcdg", "dc", "gedabc", "dec", "cdeag", "cbedaf", "gfaebc", "dafge", "bcegdaf", "agceb"] ["dfage", "bcefad", "cfbage", "fdgae"]
   , make ["gc", "cag", "agbec", "dacbe", "dcbg", "edcbgaf", "gdcfae", "dacebg", "eabfg", "febacd"] ["gc", "cbdg", "gc", "fedcba"]
   , make ["gfdbace", "edbc", "ecfgab", "efcdbg", "ce", "fdcabg", "fedga", "egc", "cbdgf", "gedfc"] ["ecdb", "edgfbc", "eagcfb", "cdafbg"]
   , make ["afceg", "bgdeaf", "daceg", "degacb", "fe", "dfec", "adfgec", "acdgefb", "gcbfa", "gfe"] ["efg", "gcfea", "dcfe", "fcgea"]
   , make ["bdca", "gcfdbea", "dc", "acbedf", "gbfde", "cagefd", "cfebag", "dcf", "bdcfe", "ebafc"] ["eafbc", "bcfage", "defcb", "agcbfe"]
   , make ["cabefg", "adgeb", "fd", "dacf", "afgcb", "bgefdca", "decbfg", "fbd", "dafgb", "dfagbc"] ["gbfca", "cbeagf", "cfbgad", "fbd"]
   , make ["bacgef", "defcga", "afdgebc", "facebd", "afebc", "badc", "bdfeg", "dc", "dbfec", "dce"] ["adbc", "eafdcb", "dabc", "fgedb"]
   , make ["cdf", "decb", "fagec", "dc", "dcgbaf", "bacfgde", "gdfeb", "efbadg", "efgbdc", "cfged"] ["ecgfa", "efgbd", "fedgab", "dc"]
   , make ["faebgd", "egdfcb", "gecadb", "gdbce", "ag", "aeg", "cgeab", "gacd", "faecb", "debcafg"] ["ag", "fdeabg", "decbg", "ag"]
   , make ["eagc", "bdegf", "ga", "gdaef", "adg", "cbgdaf", "cfaegd", "dabfce", "adcegbf", "dfeac"] ["acedfg", "agd", "cega", "bgefd"]
   , make ["cfgbe", "ceadf", "ab", "cbefag", "cegfbd", "bega", "egdfcab", "fba", "gbfacd", "efcba"] ["ceafbg", "abf", "ecfba", "ba"]
   , make ["fgdce", "ecgbaf", "gebd", "bfg", "fdbac", "dfacge", "fbegdc", "cgbdf", "dacbfeg", "gb"] ["aebcgfd", "cdabf", "ebdg", "egdcaf"]
   , make ["febda", "edagf", "egf", "gdcf", "bcgafed", "fg", "agcbfe", "caged", "gcadeb", "dgaefc"] ["cegbad", "gcdf", "gaedc", "ebdgca"]
   , make ["eabgf", "fbdcg", "dfecag", "acebgf", "bafdge", "fbgce", "ecf", "eacb", "begfacd", "ec"] ["fbgea", "gcefb", "fegcad", "decfag"]
   , make ["fabgcd", "febdac", "acgdf", "beagc", "dgaec", "de", "eafcdg", "dcfbeag", "dae", "fdge"] ["fcdgea", "ceadg", "fegd", "bdcaef"]
   , make ["bcafeg", "ecbad", "dcfe", "ecbgafd", "gdefab", "dfabce", "de", "cadgb", "ebd", "fcaeb"] ["bed", "bgeafd", "fgeacb", "ecdfba"]
   , make ["afcgeb", "gcebf", "cdbfag", "cfea", "bdgfe", "cbgaf", "ec", "gfabecd", "bedgca", "egc"] ["gce", "dacfbg", "cebfg", "afec"]
   , make ["ecdafb", "cfeag", "bdgaf", "febga", "egabfc", "eb", "gbec", "fcebdga", "eab", "dfegac"] ["bea", "gbafd", "eab", "befdac"]
   , make ["fgbacd", "fbcedga", "fgc", "fg", "gdaf", "bcdgef", "cdfba", "fabced", "cgbfa", "gcabe"] ["egcadfb", "adgf", "gbcefd", "gcf"]
   , make ["dfbcga", "caefbd", "ac", "gaedbfc", "afgc", "afgdb", "bdgce", "dgcab", "fdgaeb", "dca"] ["afbgd", "cagf", "adc", "bfedga"]
   , make ["bdgce", "dbf", "cgeadfb", "fd", "gfbde", "dfbecg", "gaebf", "dacgfb", "cdebga", "dcef"] ["fegbd", "fgbde", "bgfde", "fcegbd"]
   , make ["gbda", "egadbf", "bfegd", "acgfde", "bfgaec", "gabef", "cfbde", "aefdbcg", "dg", "ged"] ["abgefd", "gdefba", "deg", "gfeba"]
   , make ["afebcd", "degcfb", "dce", "dcfgb", "degf", "bdcgafe", "bdcge", "gcabdf", "de", "cbaeg"] ["de", "bagec", "gcdeb", "ebagc"]
   , make ["eadcfg", "dfc", "gcbfade", "cd", "edafb", "fbcge", "afdbec", "agdbef", "ecfbd", "bdac"] ["efdab", "agefdc", "befad", "defabc"]
   , make ["agbedf", "degbfc", "begcd", "ceagdf", "cgabe", "ecd", "dcbf", "ebfdg", "abecgfd", "cd"] ["fdebg", "fdgbe", "cdgeb", "bagdef"]
   , make ["aefdb", "degf", "fdbaeg", "egb", "agebd", "bcfage", "gcedfab", "bdagc", "ge", "febacd"] ["bgfdae", "fdge", "acfdeb", "deafb"]
   , make ["cegfba", "dfcea", "gbea", "bac", "ebafc", "gdcebf", "gebcf", "ba", "gfdbac", "gacfdbe"] ["febac", "aefcb", "cbfgda", "cafed"]
   , make ["ec", "ecfab", "feadbg", "ecb", "dabfc", "cbfegd", "dbafgec", "feabg", "caefgb", "ecag"] ["acbefg", "afbeg", "fecgabd", "bcfad"]
   , make ["fd", "decag", "fagbe", "beafdg", "egdacbf", "gbfcde", "dbfa", "agfbec", "dgf", "geadf"] ["bfda", "befgcda", "fgd", "adfb"]
   , make ["af", "eabfdg", "edabc", "dgfabc", "efga", "fdbae", "fcagbed", "abf", "gbcdfe", "efgdb"] ["gefa", "gfabcd", "fba", "gcdfab"]
   , make ["dbfacg", "degcb", "gecdaf", "adcbg", "bafegcd", "dfbag", "gbdefa", "ac", "cabf", "agc"] ["bcadg", "afbc", "bdgfa", "abcdg"]
   , make ["efab", "cbedg", "aedfbc", "bfcad", "geacfd", "ea", "fabdcg", "adbfgce", "ade", "ecabd"] ["fdceba", "bfceda", "ecgdfa", "aefcdg"] ]
