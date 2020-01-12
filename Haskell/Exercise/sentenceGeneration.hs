import System.Random
import System.IO
import Data.Char

{-
  Във файла words.txt са записани примерни думи. За тестване командата е: generateFromFile "words.txt" "result.txt"
-}

{-
  Задача: Да се генерира произволно изречение. Има два типа изречения: Определение Подлог Сказуемо Допълнение - Тип 1 и Определение Подлог и Определение Подлог Сказуемо Допълнение - Тип 2
  Също така да се поддържа четене на думите от файл и записването на изреченията в такъв.

  Реализация: Всяка дума се характеризира с род и число. Тогава за всяка дума ще имаме наредена тройка от (дума, род, число). Но допълненията нямат число. Поради тази причина реших да представя думите
  чрез списък от низове. Пример: ["голямото", "О", "ср"]. Така работим по еднакъв начин както с допълненията таки и с всички останали

  getRandomSentence - принтира произволно генерирано изречение от тип 1 от примерните думи
  getRandomSentence2 - принтира произволно генерирано изречение от тип 2 от примерните думи

  generateFromFile from to - генерира две изречения. Първото от тип 1, а второто от тип 2. Думите са от файла from, изреченията се записват във файла to.
-}

{-
  Примерни думи.
-}
testWords = [["голямото", "О", "ср"], ["куче", "П", "ср"], ["лае", "С", "ед"],["птиците", "Д"],["зелените", "О", "мн"],["жаби", "П", "мн"], ["гледат", "С", "мн"],["учат", "С", "мн"], ["към небето", "Д"]]

{-
  Добавяме по един спейс към всяка дума, за да работи коректно принтирането на изречения
-}
testWordsCorrected = map (\(word:rest) -> ((word ++ " "):rest)) testWords

-- Списък с определенията
testAttributes = filter (\(_:wordType:_) -> wordType == "О") testWordsCorrected

-- Списък с подлозите
testSubjects = filter (\(_:wordType:_) -> wordType == "П") testWordsCorrected

-- Списък със сказуеми
testPredicates = filter (\(_:wordType:_) -> wordType == "С") testWordsCorrected

-- Списък с допълнения
testAdditions = filter (\(_:wordType:_) -> wordType == "Д") testWordsCorrected

{-
  randomInt range
  Аргументи: 
    range - наредена двойка от две числа: (а, b), a,b - числа
  Генериране на произволно число в интервала [a, b]
-}
randomInt :: (Int, Int) -> IO Int
randomInt range = randomRIO range

{-
  choseRandom list
  Аргументи:
    list - списък
  Избиране на произволен елемент от подаден списък
-}
choseRandom :: [a] -> IO a
choseRandom list = 
    randomInt(0, (length list) - 1) >>= (\num -> return (list !! num))

{-
  generateFromFile from to
  Аргументи:
    from - път до файл от който ще се четат думите
    to - път до файл в който ще се записват изреченията.
  Генериране на две произволни изрчения. Едното е от тип 1, а другото от тип 2. Думите се взимат от файла from, а изреченията се записват във файла to.
-}
generateFromFile :: String -> String -> IO ()
generateFromFile readFrom writeTo = do
    withFile readFrom ReadMode $ \h -> do
      hSetEncoding h utf8
      content <- hGetContents h
      let linesOfFile = lines content
      let temp = map (\line -> splitBy (==' ') line) linesOfFile
      let transformedWords = map (\l -> transformLine l) temp
      
      -- Списък с определенията
      let attributesFromFile = filter (\(_:wordType:_) -> wordType == "О") transformedWords
      -- Списък с подлозите
      let subjectsFromFile = filter (\(_:wordType:_) -> wordType == "П") transformedWords
      -- Списък със сказуеми
      let predicatesFromFile = filter (\(_:wordType:_) -> wordType == "С") transformedWords
      -- Списък с допълнения
      let additionsFromFile = filter (\(_:wordType:_) -> wordType == "Д") transformedWords

      sentence <- generateFirstSentence attributesFromFile subjectsFromFile predicatesFromFile additionsFromFile
      putStrLn sentence

      sentence2 <- generateSecondSentence attributesFromFile subjectsFromFile predicatesFromFile additionsFromFile
      putStrLn sentence2
      
      writeToFileWithEncoding writeTo (sentence ++ "\n" ++ sentence2 ++ "\n")
      putStrLn "Sentences successfully added."

{-
  writeToFileWithEncoding file content
  Аргументи:
    - file - името на файла в който искаме да запишем текст
    - content - самият текст, който искаме да запишем
  Писане във файл с име file на съдържание content. Специално добавяме поддръжка на uft8 формат, за използване на кирилица.
-}
writeToFileWithEncoding :: String -> String -> IO ()
writeToFileWithEncoding file content = 
  withFile file AppendMode $ \h -> do
    hSetEncoding h utf8
    hPutStr h content

{-
  generateFirstSentence attributes subjects predicates additions
  Тъй като всяка дума освен нея имаме и какво число е както и какъв род, използвам списък от списъци, за да запазя цялата информация.
  В случаят имаме [[String]] - елемент([String])
  Аргументи:
    - attributes - списък съдържащ всички елементи, които са определения
    - subjects - списък съдържащ всички елементи, които са подлози
    - predicates - списък съдържащ всички елементи, които са сказуеми
    - additions - списък съдържащ всички елементи, които са допълнения
  Генерира произволно изречение съставено от първи тип: Определение Подлог Сказуемо Допълнение
-}
generateFirstSentence :: [[String]] -> [[String]] -> [[String]] -> [[String]] -> IO String
generateFirstSentence attributes subjects predicates additions = do
  subjectElement <- (choseRandom subjects)
  subjectWord <- getWord subjectElement 
  subjectNumber <- getWordNumber subjectElement
  attribute <- (generateAttributeWithConstraint subjectNumber attributes) >>= getWord
  predicate <- (generatePredicateWithConstraint subjectNumber predicates) >>= getWord
  addition <- (choseRandom additions) >>= getWord
  let sentence = getSentenceFirstType attribute subjectWord predicate addition
  return sentence

{-
  generateSecondSentence attributes subjects predicates additions
  Тъй като всяка дума освен нея имаме и какво число е както и какъв род, използвам списък от списъци, за да запазя цялата информация.
  В случаят имаме [[String]] - елемент([String])
  Аргументи:
    - attributes - списък съдържащ всички елементи, които са определения
    - subjects - списък съдържащ всички елементи, които са подлози
    - predicates - списък съдържащ всички елементи, които са сказуеми
    - additions - списък съдържащ всички елементи, които са допълнения
  Генерира произволно изречение съставено от втори тип: Определение Подлог и Определение Подлог Сказуемо Допълнение
-}
generateSecondSentence :: [[String]] -> [[String]] -> [[String]] -> [[String]] -> IO String
generateSecondSentence attributes subjects predicates additions = do 
  additionSecondType <- (choseRandom additions) >>= getWord
  predicateSecondType <- (generatePredicateWithConstraint "мн" predicates) >>= getWord
  subjectElement1 <- (choseRandom subjects)
  let newSubjects = removeElementFromList subjects subjectElement1
  subjectElement2 <- (choseRandom newSubjects)
  subjectWord1 <- getWord subjectElement1
  subjectNumber1 <- getWordNumber subjectElement1
  subjectWord2 <- getWord subjectElement2
  subjectNumber2 <- getWordNumber subjectElement2
  attribute1 <- (generateAttributeWithConstraint subjectNumber1 attributes)
  let newAttributes = removeElementFromList attributes attribute1
  attributeWord2 <- (generateAttributeWithConstraint subjectNumber2 newAttributes) >>= getWord
  attributeWord1 <- getWord attribute1
  let sentence = generateSecondType attributeWord1 subjectWord1 attributeWord2 subjectWord2 predicateSecondType additionSecondType
  return sentence

{- 
  Форматиране на изречение от тип 1.
-}
getSentenceFirstType :: String -> String -> String -> String -> String
getSentenceFirstType attribute subject predicate addition = [(toUpper (attribute !! 0))] ++ (drop 1 attribute) ++ subject ++ predicate ++ (init addition) ++ "."

{-
  Форматиране на изречение от тип 2.
-}
generateSecondType :: String -> String -> String -> String -> String -> String -> String
generateSecondType attribute1 subject1 attribute2 subject2 predicate addition = [(toUpper (attribute1 !! 0))] ++ (drop 1 attribute1) ++ subject1 ++ "и " ++ attribute2 ++ subject2 ++ predicate ++ (init addition) ++ "."

{-
  removeElementFromList l el 
  Аргументи:
    l - списък от който искаме да махнем елемент
    el - елементът, който искаме да премахнем
-}
removeElementFromList :: Eq a => [a] -> a -> [a]
removeElementFromList l el = filter (\x -> el /= x) l

{-
  Взимане на думата от елемента
-}
getWord :: [String] -> IO String
getWord (word:_) = return word 

{-
  Взимане на рода на думата от елемента
-}
getWordType :: [String] -> IO String
getWordType (_:wordType:_) = return wordType

{-
  Взимане на числото на думата от елемента
-}
getWordNumber :: [String] -> IO String
getWordNumber (_:_:wordNumber:_) = return wordNumber

{-
  Преобразуване на списък от низовете в реда на файл в списък от вида на елемента, а именно: ["дума", "род", "число"]
-}
transformLine :: [String] -> [String]
transformLine l 
  | (l !! (len - 1)) == "Д" = (concatenate l (len - 1)) : (drop (len - 1) l)
  | otherwise               = (concatenate l (len - 2)) : (drop (len - 2) l)
 where len = length l

{-
  Слепване на N елемента на списък
  Пример: ["към" "небетo" "Д"] 2 => "към небето"
-}
concatenate :: [String] -> Int -> String
concatenate elements 0             = []
concatenate (elem:elements) number = elem ++ " " ++ (concatenate elements (number - 1))

{-
  splitBy p x
  Аргументи: 
    - p - Предикат, който сравнява дали символ от думата е равен на конкретен символ
    - x - Низ, който искаме да разделим спрямо предиката
  Разделяне на низ на списък от низове спрямо символът от предиката.
-}
splitBy :: (Char -> Bool) -> String -> [String]
splitBy _ [] = []
splitBy p x  = 
  let next = (getUntilFirst p x) 
      len = length next 
  in next : splitBy p (drop (len + 1) x)

{-
  getUntilFirst p x
  Аргументи: 
    - p - Предикат, който сравнява дали символ от думата е равен на конкретен символ
    - x - Низ, който искаме да разделим спрямо предиката
  Връща низът до първото срещане на символа от предиката.
-}
getUntilFirst :: (Char -> Bool) -> String -> String
getUntilFirst _ []     = []
getUntilFirst p [x]    = if p x then [] else [x]
getUntilFirst p (x:xs) = if p x then [] else x : getUntilFirst p xs

{-
  generatePredicateWithConstraint constraint l
  Аргументи:
    - constraint - низ, който е ограничението ни спрямо подлога.
    - l - списък от всички сказуеми като елементи
  Генериране на произволно сказуемо, което да отговаря на дефинираното ни правило.
-}
generatePredicateWithConstraint :: String -> [[String]] -> IO [String]
generatePredicateWithConstraint constraint l
  | constraint == "мн" = choseRandom (filter (\(_:_:wordNumber:_) -> wordNumber == constraint) l)
  | otherwise          = choseRandom (filter (\(_:_:wordNumber:_) -> wordNumber /= "мн") l)

{-
  generateAttributeWithConstraint constraint l
  Аргументи:
    - constraint - низ, който е ограничението ни спрямо подлога.
    - l - списък от всички определения като елементи
  Генериране на произволно определение, което да отговаря на дефинираното ни правило.
-}
generateAttributeWithConstraint :: String -> [[String]] -> IO [String]
generateAttributeWithConstraint constraint l = choseRandom (filter (\(_:_:plurality:_) -> plurality == constraint) l)

{-
  Принтира на стандартния изход произволно генерирано изречение от тип 1 от примерните думи
-}
getRandomSentence :: IO ()
getRandomSentence = (generateFirstSentence testAttributes testSubjects testPredicates testAdditions) >>= putStrLn

{-
  Принтира на стандартния изход произволно генерирано изречение от тип 2 от примерните думи
  П.С. твърде малко са примерните думи за да се генерира различно изречение.
-}
getRandomSentence2 :: IO ()
getRandomSentence2 = (generateSecondSentence testAttributes testSubjects testPredicates testAdditions) >>= putStrLn
