{-# LANGUAGE LambdaCase #-}

module AdventOfCode.Y2021.Day10 where

import AdventOfCode.Common.Parser
import Data.Maybe
import Data.List

--------------------------------------------------------------------------------

data ChunkType = Parens | Bracket | Brace | Angle
   deriving (Show, Eq, Enum, Bounded)

data CorruptedChunk = CorruptedChunk ChunkType [CorruptedChunk] ChunkType
   deriving (Show, Eq)

getChunkType :: Char -> ChunkType
getChunkType c
   | c `elem` "()" = Parens
   | c `elem` "[]" = Bracket
   | c `elem` "{}" = Brace
   | c `elem` "<>" = Angle

isOpen :: Char -> Bool
isOpen c = c `elem` "([{<"

isClose :: Char -> Bool
isClose c = c `elem` ")]}>"

getCounterpart :: Char -> Char
getCounterpart = \case { '(' -> ')'; ')' -> '('; '[' -> ']'; ']' -> '['; '{' -> '}'; '}' -> '{'; '<' -> '>'; '>' -> '<' }

--------------------------------------------------------------------------------

openParser :: Parser ChunkType
openParser = do
   c <- char
   guard (c `elem` "([{<")
   return $ getChunkType c

closeParser :: Parser ChunkType
closeParser = do
   c <- char
   guard (c `elem` ")]}>")
   return $ getChunkType c

corruptedChunkParser :: Parser CorruptedChunk
corruptedChunkParser = do
   a <- openParser
   s <- many corruptedChunkParser
   b <- closeParser
   return $ CorruptedChunk a s b

getCorruption :: String -> (Maybe Char, String)
getCorruption [] = (Nothing, "")
getCorruption (c:s) | isClose c = (Nothing, c:s)
getCorruption (c:s) | isOpen c = case getCorruption s of
   (Just x, r) -> (Just x, r)
   (Nothing, []) -> (Nothing, "")
   (Nothing, x:r) -> if x == getCounterpart c then getCorruption r else (Just x, r)

score :: Char -> Int
score = \case { ')' -> 3; ']' -> 57; '}' -> 1197; '>' -> 25137 }

part1 :: [String] -> Int
part1 lines = sum $ map score $ catMaybes $ map (fst . getCorruption) lines

--------------------------------------------------------------------------------

discardCorrupted :: [String] -> [String]
discardCorrupted = filter (isNothing . fst . getCorruption)

autocomplete :: String -> (String, String)
autocomplete "" = ("", "")
autocomplete (t:s) | isClose t = ("", t:s)
autocomplete (t:s) = case autocomplete s of
   (eol, "")  -> (eol ++ [getCounterpart t], "")
   (_, (_:r)) -> autocomplete r

autocompleteCharScore :: Char -> Int
autocompleteCharScore = \case { ')' -> 1; ']' -> 2; '}' -> 3; '>' -> 4 }

autocompleteScore :: String -> Int
autocompleteScore = foldl (\s c -> s * 5 + autocompleteCharScore c) 0

part2 :: [String] -> Int
part2 lines = let
   scores = sort $ map (autocompleteScore . fst . autocomplete) $ discardCorrupted lines
   n = length scores `div` 2
   in scores !! n

--------------------------------------------------------------------------------

sampleInput :: [String]
sampleInput =
   [ "[({(<(())[]>[[{[]{<()<>>"
   , "[(()[<>])]({[<{<<[]>>("
   , "{([(<{}[<>[]}>{[]{[(<()>"
   , "(((({<>}<{<{<>}{[]{[]{}"
   , "[[<[([]))<([[{}[[()]]]"
   , "[{[{({}]{}}([{[{{{}}([]"
   , "{<[[]]>}<{[{[{[]{()[[[]"
   , "[<(<(<(<{}))><([]([]()"
   , "<{([([[(<>()){}]>(<<{{"
   , "<{([{{}}[<[[[<>{}]]]>[]]" ]

myInput :: [String]
myInput =
   [ "{[<<([{((((<[<[]{}><[]{}>]<<{}{}><[]<>>>><<([]<>)>[({}[])({}[])]>))([[({[]{}})<{{}{}}(())>][[{"
   , "({[{({[<<<<<<(()())>>{{([]{}){{}[]}}{<(){}><<>[]>}}>{(<[[][]]{<><>}><<{}<>>{<>{}}>)}>>>]<[{<{<{[(){}]<()()>"
   , "[{[{{{[<({<((<()()>[{}()])({<>[]}{[]()}))>[{{[[]()]<{}()>}[<[]<>>{<>{}}]}{[<{}<>>{<>()}][(()<>)["
   , "{([<(<{{([[{(((){})[<>[]])<[[]{}][{}<>]>}{[{<>{}}]{<[]()><()[]>}}]])}({[<{(<()<>>)({{}()}["
   , "<[{({{(({<[{{(<>()){[]{}}}[{[]{}}]}<{{{}{}}<{}()>}{<<><>>({}())}>](<{{[]()}[{}[]]}((()<>>([]<>)"
   , "({[{[[[{{{{[<({}())[[]()]>({<><>}(<>()))]<((()())<{}<>>){{<><>}(()())}>}{<({[][]}<[]()>){{{}()}"
   , "{((<<<<(([(((({}()))[[()()]{{}{}}])(<{{}}{()<>}>((<>[]){{}<>})))({<({}())[<>[]]>[{[]<>}]}[<[{}{}]<"
   , "(({([{{{{([<{<{}[]>({}{})}[{<>{}>([]{})]>][(((()())<()[]>)(<(){}>[{}{}]))(({{}{}}({}()))({{}("
   , "{([<{{{{({{{([(){}])}[<<[]<>>([][])>({[]<>}{[]()})]}}{{[{(<>[]){(){}}}({[]()}{{}{}})]{{{{}()}([]<>)}}}{{"
   , "(<<(<<{[<{((({[])<[]{}>){[[][]]}))[[<<<>{}>>][({{}()})([()[]]<[]<>>)]]}(({<[()()]{()[]}>([<>{"
   , "({<[({[<{[{(<<()[]>(<><>)>{([][]){()<>}})[(<(){}><{}[]>)(<[]{}>[<>[]])]}]<(<[(()[])[{}[]]]>{[[(){}]([]"
   , "<<<{{<([([<(<<{}{}>[<>[]]>(<{}()>[[][]])){<<<>{}>([]())>(([]{}){[]})}><{{{{}[]}<()()>}(<{}<>>{[]<>}"
   , "<[[{{<(<[{[{{({}[]){()()}}((()())<()<>>)}][[{(<>[])(<><>)}(<()[]>{{}<>})]]}[{<<<{}<>>><{{}<>}[{}{}]>><"
   , "[<{([<[<<<<[({<>[]}{()}){[(){}]{()[]}}]><[[[[][]]{<><>}]]<<{()<>}{<>{}}>(<()()>[{}()])>>)<<{{<<>"
   , "[([(({([({{<<{<><>}>({{}{}}{(){}}))<[<{}{}>(()<>)]<<[]>>>}}{({<<{}<>>{[]()}>{[{}[]]{()[]}}"
   , "{{<<(({{[[<({{(){}}}[<<>{}>{[][]}])<{<{}<>><{}{}>}{(<><>)((){})}>>{[{(<>[])([]{})}{<{}[]>}]}]<<[<((){})(<"
   , "<{(<<([<[{<{{{()[]}<<>{}>}(({}<>)(<>()))}{{((){})([]<>)}<{()<>}[<>[]]>}>{[<{{}<>}>(<<>[]>[{}"
   , "<{({(<{<<{(([(<>[]){<><>}]({[]{}}(()[]))))([[{{}()}{{}<>}][[[]()]{()}]])}(<[<<[]()>(<>{})){{{}()}<<>()>}"
   , "<<([{((<({<[<({}[])({}[])>]<{[[]<>]<()[]>}[<{}<>>(()<>)]>>[[<[<>()]<()>>{(<><>]{[]<>}}][{<()<>>{[]<>}}"
   , "[{<[<(<[(<{<({{}{}}[{}[]])><{(()<>){[]()}}{{[]()}[()()]}>}>[[[<({}[])>{<()<>>[<><>>}]]])]>(<{<<[<("
   , "[[(<<<{{[[<<<[[]<>][[]<>]>{{<>{}}}><<{[][]}{()}>>>]]}[([{<<[{}{}]<{}<>>>>[(<()[]>{()()})]}]([[(<"
   , "<<<[<<[<[{<(({{}{}}<[]{}>){[{}{}]{{}{}}}}{<(<><>)[{}{}]>{[(){}][()<>]}}><(<{<>{}}(<>{})>{<{}<"
   , "[({<[<(((<({[[{}{}](()[])]<<[][]}>}<{{[]{}}([]())}{(<>())<{}[]>}>)>))({<[<{(<>{})[<><>]}(<(){}"
   , "<(({[<[<{{{<[[<>{}]]{((){}){<>[]}}>(([{}<>][{}<>])[[[]()]<()()>])}(<[<[]{}>{<>{}}]><<(<>()){<>()}>([()[]]{<>("
   , "<<<([(<(<(<[<{<><>}<()[]>>(((){})((){}))]<({()[]}({}<>)){<[]<>><<>()>>>>[<[[<><>]([]<>)]>[(({}{})<()"
   , "(([[(<{[([({[[{}()]{[]{}}][{{}<>}<{}<>>]})([[[<><>]<<>>]][([[][]])])][<<[[()<>]{<>{}}]<[{}()](()())>>{[("
   , "[{{<<{<({({[(<<><>><<>[]>){[[]{}](()())}][((<><>){<>[]})(({}<>){[]()})]}<(({[]()})(<[]()><{}[]>))(<"
   , "{(<{<[<[<[<[[<()[]>(<>{})]{(<>{})<<><>>}]>[({[<><>]})[<<{}<>>{<>{}}>]]]<[[<{{}()}(<>[])>]]<{{<()[]"
   , "{[{((<<<<[<{({()<>})}>{<(({}<>)){{()()}{{}{}}}>[<[{}()][<>()]>[[<>{}]{[]()}]]}]({{[<<>{}>(<>[])"
   , "(([[<<({{{[<<([]{}){(){}}>[({}<>)<{}<>>])(({[][]}){{<>{}}[[]()]})]}}})([((((({[]()}[()<>]))"
   , "<<<{<<[[{({([{()()}(())])<<{[]<>}<()[]>><<()[]>[{}<>]>>})}<[[{([<>[]>(()())){([]{})<()[]>}}<[<(){"
   , "{<{{{<[([[{[<(<>)<<><>>>{<<>{}>({}[])}](({()<>}({}))[(<>[])])}]<{[[<{}>(<>[])]{[[][]][<>[]]]]{((<><>)[(){"
   , "<({({<<(({([[[()[]]{<><>}]{[[]<>]}]{[<<><>>[(){}]]({()[]){{}{}})})}<{<[[[][]]<<>{}>](({}<>)(()<>))>"
   , "{{{[(((<{{(<({{}[]})><([{}{}])<([][])[<>[]]>>)<{[(()<>)[<>[]]]{[{}[]]<[]()>}}>)}{<<[<<[]{}>[{"
   , "[({<{(([<<([([[]()]<{}[]>)<{{}()}<<>[]>>]{[{{}}<[]>]{{[][]}(<>())}})[[{<[]{}>{()<>}}<(<>{})(<>{})>]["
   , "{<(<{[{[{(((<{{}[]}{[]()}>)[{{<>[]}{<>[]}}((()[])<<>()>)]))(({[{<><>}<[]{}>][{[]()}]}<<{<>}[<>()]>(<[][]>{[]{"
   , "{{{([[{<<[{[{[[]()]{{}{}}}([{}]{()[]})][<<[][]>(<>{})>]}]{[<<[<>())(<><>)>[{<>{}}{[]<>}]>]"
   , "{[<{{<[[<[[(<([][]){()}>[{{}<>}[[]<>]])<((<><>){<>{}})([<>{}]{{}[]})>](<[{()<>}[<>{}]]>{<<[]"
   , "[<(((({[{((<{<{}{}><(){}>}][<(()<>)<[]{}>>(<()[]>[{}{}])]){[[[[]()]([]<>)]][<(()[])<<><>>><{[]<>}>]}"
   , "({(<<(({{[{<<{{}()}{()<>}>({[][]}{<>{}})>}]([{{{[]{}}<{}()>}[(()[])({}())]><{[{}<>]<()<>>}["
   , "({(<<[({({<[<[()()]>{({}()){{}{}}}]>[<{(()<>)<{}{}>}{<{}{}>[<>[]]}]((<[]<>>)[[[]()]<<>[]>])]})[[[[[["
   , "{<(<<[[[<{({{{[][]}{()}}[{{}<>}{[]<>}]}<([[]{}]{[]()})[(()<>)([][])]>)}>]]]>>[<{[<{[<[([{}{}]([]<>))[["
   , "{<({{<{<(<[{[[{}()](<>{})]<{<>()}{<>[]}>}{{[{}{}]}<<{}{}>({}[])>}]({[{()()}[()[]]]}[<{()()}{{}{}}>[<()[]"
   , "{[[{(([({([([{<>()}[<>{}]])]<<[((){})({}<>)]><[[[]<>](<>[])][<()>[[]()]]>>)})])<[{<[<<<[[]]>{<<><"
   , "{<[[<[<<{[{<({[][]}[(){}]){<()<>>[{}<>]}><([[]{}][()[]])[<()()><()[]>]>}({[{[][]}][[{}{}]]}[[{(){}}<<>[]>"
   , "({(<{[[{<<<(([{}()]<(){}>)[[{}<>]<<>[]>]){<{[]<>}(()<>)>}><{({[][]}(())){<()()>(<><>)}}>>([<([{}[]][<>()"
   , "<{<(([(<({[<{((){})<{}()>}[[[]<>]{[]()}]>({({}[])<(){}>})]{[{[()]{{}[]}}<[{}()][[]{}]>]}})>)[<<([({<"
   , "[(({{{(<{{{[<<[]()><[]<>>>{((){})}]}{{{<()<>><[][]>}}}}{[<(<[]<>>[{}[]])[{()()]]><[([]())[{}[]]]({{}[]}(()["
   , "[[(<<<{{<{<<([<>()]{{}{}})[[<><>][<>[]]]>>[[([<>[]][[]{}])({[]{}})]<[<{}{}>[()<>]]>]}{(<[{{}[]}{"
   , "{{{(<[(<{(<[{{<>()}{[]<>}}]>)<[{([[]()]<{}<>>)[{<><>}([]<>)])(<{()}(()<>)>([{}<>](<>[])))]<[<<<>><<><>"
   , "<[[(([(<<[{[<<()[]><<>()>>{[{}[]][()[]]}]{[<()[]>{()<>}](<()()><{}[]>)}}{({[[]](()<>]}({[]["
   , "<[<{{<([<{<{[{[]}({}<>)][(<>())]}<<[()[]](()[])>{<{}()>[()[]]}>>(<{((){})[{}()]}>((((){})((){"
   , "([<{<{[<[[<<{(<>[])}[<<>[]>{[][]}]>>]<({<(()())([]())>(({}<>)<[]()>)}{({(){}}([]{}))[{<><>}<<><"
   , "{{(([({({{<(({{}()}{[]{}})<([]{})>)>[[[[{}<>]](((){})[()<>])][(({}{})({}[]))<[[][]]<<>{}>>]]}))}){{<"
   , "[{{<{{<<{({{<[{}[]][[]()]>[<<><>><{}{}>]}{[<()[]><[][]>]({{}()}(<>{}))}}<<([<><>][[]<>]){(<>}(<>())}"
   , "{[<({<[[[[{<{<<>{}><<><>>}{[<><>]({}<>)}>((((){})[{}<>])[{()<>}{{}[]}])}({(<[]()><{}()>)}{("
   , "[((((({{<[{({[{}[]]{{}()}}{(<>())[[]<>]})}(<{<()[]><[]{}>}>{<[[]()]<(){}>><{[]}{[][]}>})][<{(("
   , "{[(((([[[([{<{()[]}(()[])>((()<>)[<><>])}]([([{}()][[]()])[{{}<>}{(){}}]]({({}{})<{}{}>}<{{}[]}<{}<>>>)"
   , "[(<([{{[(<({{[<>]<()<>>}{[[]{}][[]{}]}}]>)]}}<{(([[{({[]()}({}[]))<[()<>]{(){}}>}]{({(<>[]"
   , "(<([([<([<{{{{<><>}<()()>}{({}()]<[]<>>}}}>]([({<((){}){{}[]}>}([{()()}{<>{}}]<{[]<>}{(){}}>)){{[<[]("
   , "[{<({{([(<[(<{{}()}{<>()}>((<>())<<>()>))<<[{}[]]><({}())[<><>]>>]<{{[{}{}]{[]()}}<(()<>}<()[]>>}<<({}()"
   , "[<<(([<{<{[[{{[]<>}{{}()}}<[<>()]<<><>>>]({({}())(())}([{}{}]<{}()>))]{<((<><>){<><>}){(()())}>}}({{(["
   , "{{{([{{[<{(<{<<>{}><<>()>}<{()<>}{<>[]}>>)[[<[<>{}]<{}{}>><{()[]}>][(({}())([]{}))[[{}[]][{}()>]]]}["
   , "({<(({<<(<<[{<<>{}>{<><>}}((())<{}[]>)]><{((<>{})<<>()>)}>))>(<[<[{[<>{}][[]()]}[({}{}){<>()"
   , "[{{<[<<({([{{{()[]}(<><>)}([<><>]<<><>>)}[({<>{}}((){}))]][[(<<>{}>[{}()])<([]())([]()>>]{"
   , "({([<[((([{([<()()>[()<>]])(<{[][]}<{}[]>><[()<>]{<>[]}>)}({[{<>{}}{[]{}}][([][])]}[{{<><>}}<[{}{"
   , "{[{(<[[[{<([({{}()}(<>[]))<[[]<>][[]<>]>][<(<><>)<<>{}>><[[]{}]{<>{}}>])(([<[][]>{{}()}]<{"
   , "{<<(<<{<<([{[<{}[]><{}[]>]{{{}{}}{[]()}}}(<[[]{}][[]<>]><[()()]>)]){(([(<><>)([][])](<{}()>)){([<>{}](<>"
   , "(([[{{[[{{{<{{<>[]}{()[]}}(<()<>})>({[[]()]{[]()}}<(<>())>)}[[([{}](()[]))<<<>{}>{<>{}}>]{{({}<>"
   , "[<([(<<{({<[<[(){}]{(){}}><<{}[]>(<>())>]>({([<><>]({}<>)){([])[{}[]]}}[[<[]><{}>]])}){<{<<{[]<>}[{}[]]>>}{(("
   , "((<<({({({[[[<{}[]>{[][]}]<{[][]}[<>{}]>]{[([]{}><[]{}>]}]((<{()[]}[<>[]]>{(<>[]){()<>}}))}[[["
   , "(((<<{{([{{(([[]<>]<[][]>)({[]{}}[()<>]))[<({}{})<[]{}>>{{{}()}[()<>)}]}}[{<{<(){}>[()]}{(<>{}){()[]}}>"
   , "([([<<<[[[[<{{[]()}}[[<>()][[]()]]>{({{}{}}<[]()>)}]]][({([({}{})[{}<>]])[{<[]>{()()}}{(()[])"
   , "{[<(({((<{([{(<>[])}]{{{<>}[[][]]}((())<[]{}>)})}>[{([[<{}<>>([]{})]))[{<{{}<>}[()[]]><{[][]}"
   , "<[[<{[<[{{{[<(()())}([[][]]<<>{}>)]{<{[]}<{}[]>><[{}{}](<>{})>}}}}({(<(([]()){[]()})><{({}<"
   , "[(({({[(<({(((()[])))})>[[{(([[]()](()<>)){{{}<>}<{}()>}){[<(){}><[]()>]<({}<>)[()()>>}}[[<<()()>{()[]}>"
   , "{<[({([[(({[{[<>{}]{<>{}}}{[<>()]}][<[{}[]]<()<>>>{<{}()><{}{}>})}(<{[<><>][<><>]}>([(<>{})<{}()>"
   , "[{<{<{{([[[[[([])({})](([]()){<>{}})]<[{{}{}}[{}{}]][<()()>(<>{})]>]]]([([<[{}[]]<()>>[({}[])(<>())]]("
   , "<(<{({{((<<[({[][]}[<><>])]><<{[{}]<[]<>>}[<[]<>>]>([{{}[]}({}<>)][[[][]]{[]()}])>>((([{(){}}[(){}]])({<()<>"
   , "<{(<<(<({[<{([{}]{[]{}})[<()[]><{}[]>]}>(([({}[])]<(()<>)[()()]>)<[{[]()}<<>()>](<(){}>[{}{}])>)]}<{({[{[]<>}"
   , "[(({([({[<[{({[][]}<()()>)<(<>{})[{}()]>}{<{{}<>}<{}[]>>([<>()](<>))}]>[[((<[]>[[]<>]){({}())((){})})][([[("
   , "({[[(<<{{{<[[[()<>]{{}<>}]{<()[]>({}())}]<[[{}[]]{<>{}}][[<>{}]{[][]}]>>}}}]>[<{{<{[[<<>[]>({}{"
   , "((<[{[{{[<<{<{{}<>}<()<>>>[<{}<>>(()())]}(<<{}()><[]()>>(<{}{}>{(){}}))>([({<><>}<{}()))[{["
   , "([{{[{[{[(([[(()[])[<><>]](<{}()>([]<>))]))(({[{()}]<(<>[]){()<>]>}[({()[]}[{}()])]))]}][<(<{<<[[]<>]<(){}"
   , "{{<{([{<[([<{(<>[])<{}{}>}[{(){}}(<>())]>[(<{}<>>[{}<>])((<><>)[()[]])]]<(({{}{}}<{}[]>)<[{}"
   , "{{{{({<[{({{[[<>{}][[][]]]<<()<>>{[]}>}})]]>}){[<[<({((<{}[]>({}()))((()<>)))((<[]<>>))})>[([{[<{}()>(<>{})]<"
   , "{(((((({((([{((){})([]<>)}(<{}{}>(<>()))]<<{<>[]}<<>()>>>){{<([]())[{}{}]>[(()())<(){}>]}<<{{"
   , "{[[{<{(([{[[{(()[]){[][]}}(<()<>){{}()})]]}<(<([(){}])[[{}[]]{<>()}]>){{<[[]()]>(([]<>)[[]<>])}{(<{}<>>[(){}"
   , "<<[<{<[[({(<<(()()){{}{}}>>[{<(){}>({}())}{<{}()>[[]<>]}])[{{({}<>)<<>()>}<{{}()}<{}<>>>}[{[{}{}]"
   , "([([{{[[([<[[<<>{}>[[]()]]{[[]<>]<[]<>>}]({[<><>][<>{}]}{[[]()]{<><>}})}<(<<<>[]>{{}()}>{<()[]>"
   , "[[[[([<<{[{({[{}<>][{}<>]}([()[]]<[][]>))(<([])>)}{{({<><>}[{}()])(<<>()><[]()>)}{((<>[])<"
   , "[(<[([<<({[<<({}[])(()[])><{<>()}{{}<>}>>(<{[]<>}{{}[]}>{[()()]<[][]>})][[{<{}[]>(()[]>}<<{}[]"
   , "[{[<<({([{<[({[][]})]>{({{()<>}{{}[]}}[(<><>){[][]}])}>]<[<<<<[]()>(()[])>{[[]()]([]<>)}>>(<"
   , "<[(({<[({<[{{{()()}<<>[]}}({<>[]}<[]<>>)}]{[<<[]()><()()>>[({}<>){[]{}}]][<<()()>[<>]>{{()<>}}]}>})]>}))({" ]