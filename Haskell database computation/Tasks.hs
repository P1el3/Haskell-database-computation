
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use isJust" #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Eta reduce" #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array
import Distribution.Simple.Program.HcPkg (list)
import Text.Read

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}


-- Task 1

compute_average_steps :: Table -> Table
compute_average_steps m = ["Name", "Average Number of Steps"] : map calculate_sum (tail m)

calculate_sum :: [String] -> [String]
calculate_sum x = head x : [aaverage( to_Int ( tail x))]

{- functie care primeste o lista de int-uri, calculeaza suma lor,
iar rezultatul il imparte la 8 pt a obtine media
apeland ulterior functia de print, pentru a afisa media cu 2 zecimale -}
aaverage :: [Int] -> String
aaverage x = fprint ( fromIntegral( sum x) / 8)

--functie de printare
fprint :: Float -> String
fprint = printf "%.2f"

--functie care-mi face cast de la o lista de stringuri la una de int uri
to_Int :: [String] -> [Int]
to_Int = map read


-- Task 2

-- Number of people who have achieved their goal:
get_passed_people_num :: Table -> Int
get_passed_people_num m = length (filter (\x -> do_sum(to_Int (tail x)) > 1000) (tail m))

--functie care-mi calculeaza numarul de pasi facuti in total
do_sum :: [Int] -> Int
do_sum [] = 0
do_sum x = sum x


-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = float_2dec (to_float (get_passed_people_num m) / to_float (length (tail m)))
--impart numarul de persoane care si-au atins nr de pasi la nr total de persoane si inmultesc cu 100

--pt a printa rezultatul in 2 zecimale, am creat functia
--care rotunjeste nr meu ex (0.7342414 * 100) +round = 73 / 100 = 0.73
float_2dec :: Float -> Float
float_2dec x = to_float(round(x * 100)) / 100

--functie care mi face cast de la int la float
--probabil mergea sa fac si fara ea
to_float :: Int -> Float
to_float = fromIntegral


-- Average number of daily steps
get_steps_avg :: Table -> Float
get_steps_avg m = to_float (sum (map (no_steps . tail )(tail m))) / to_float (length (tail m))

no_steps :: [String] -> Int
no_steps = sum.map read


-- Task 3

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = [["H10","H11","H12","H13","H14","H15","H16","H17"], average m]

--calculez media pasilor
average :: [[String]] -> [String]
average x = map (\y -> fprint (int_to_float y / int_to_float(length (tail x)))) (shortcut x)

--fac cast de la int la float
int_to_float :: Int -> Float
int_to_float x = fromIntegral x :: Float

--combin functiile
shortcut :: [[String]] -> [Int]
shortcut x = sum_days (string_to_int (trans x))

--calculez nr de persoane
no_pers :: [[String]] -> Int
no_pers x = length (tail x)

--fac suma pe linii
sum_days :: [[Int]] -> [Int]
sum_days [] = []
sum_days x = sum (head x) : sum_days (tail x)

--transform in int-uri
string_to_int :: [[String]] -> [[Int]]
string_to_int x = map( map (\y -> read y::Int).tail)(tail x)

--transpun matricea pe care o primesc
trans :: [[a]] -> [[a]]
trans ([]:_) = []
trans mat = map head mat : trans (map tail mat)

-- Task 4

--aici am invatat de puterea $ -ului

get_activ_summary :: Table -> Table
get_activ_summary m = [["column","range1","range2","range3"],"VeryActiveMinutes" : appling_check_r1 m,
                        "FairlyActiveMinutes" : appling_check_r2 m, "LightlyActiveMinutes" : appling_check_r3 m]

--fac elementele unei liste de int-uri in string-uri
int_to_string :: [Int] -> [String]
int_to_string  = map show

--aplic conditia pe al 3-lea sir
appling_check_r3 :: [[String]] -> [String]
appling_check_r3 x = int_to_string $ checking $ head $ tail $ tail $ prepare_for_sum x

--aplic conditia pe al 2-lea sir
appling_check_r2 :: [[String]] -> [String]
appling_check_r2 x = int_to_string $ checking $ head $ tail $ prepare_for_sum x

--aplic conditia pe primul sir
appling_check_r1 :: [[String]] -> [String ]
appling_check_r1 x = int_to_string $ checking $ head  $ prepare_for_sum x

--aici innumar aparitiile
checking :: [Float] -> [Int]
checking [] = [0,0,0]
checking x
  | head x < 50.0 = zipWith (+) [1,0,0] (checking $ tail x)
  | head x >= 50.0 && head x < 100.0 = zipWith (+) [0,1,0] (checking $ tail x)
  | otherwise = zipWith (+) [0,0,1] (checking $ tail x)

--fac elementele din string in float
--si scurtez lista la cele 3 coloane de care am nevoie
prepare_for_sum :: [[String]] -> [[Float]]
prepare_for_sum x = tail $ tail $ string_to_float $ rem_name_header x

--transpun matricea si scap de nume si de header-uri
rem_name_header ::[[String]] -> [[String]]
rem_name_header x = tail $ trans (tail x)

--transform o lista de liste de stringuri intr-una de float-uri
string_to_float :: [[String]] -> [[Float]]
string_to_float = map( map (\y -> read y :: Float))

-- Task 5
get_ranking :: Table -> Table
get_ranking m = ["Name","Total Steps"] : map (take 2) (byNoSteps m)

--cu ajutorul "!!" iau cel de-al doilea element dupa care compar
--daca cele 2 valori sunt egale, compar capetele 
--(initial facusem o functie care sa compare litera cu litera, but..)

byNoSteps :: [[String]] -> [[String]]
byNoSteps x = sortBy op (tail x)
                where op s1 s2
                            | str_int (s1!!1) < str_int (s2!!1) = LT
                            | str_int (s1!!1) > str_int (s2!!1) = GT
                            | otherwise = compare s1 s2

--transform un string intr-un int
str_int :: String -> Int
str_int = read

-- Task 6

get_steps_diff_table :: Table -> Table
get_steps_diff_table m = ["Name","Average first 4h","Average last 4h","Difference"] : recicle_t5 (map nume_medie_dif m)

--task 5 reciclat
recicle_t5 :: [[String]] -> [[String]]
recicle_t5 x = sortBy op (tail x)
                where op s1 s2
                            | strflt (s1!!3) < strflt (s2!!3) = LT
                            | strflt (s1!!3) > strflt (s2!!3) = GT
                            | otherwise = compare s1 s2

--imi face o lista de tipul [Nume, Medie, Medie, Diferenta]
nume_medie_dif :: [String] -> [String]
nume_medie_dif x = (head x) : (afisez_mediile x) ++ (diferenta_mediilor x)

afisez_mediile :: [String] -> [String]
afisez_mediile x = float_to_str  $ dividee $ str_to_flt (tail x)

diferenta_mediilor :: [String] -> [String]
diferenta_mediilor x = float_to_str  $ diff $ dividee $ str_to_flt (tail x)

--o alta fct de str to float, ca asta de o facui am bulit-o
strflt :: String -> Float
strflt = read :: String -> Float

float_to_str :: [Float] -> [String]
float_to_str = map fprint

--face cele 2 coloane
diff :: [Float] -> [Float]
diff x = [abs(head x - x!!1)]

--calculez media pe cele 2 intervale de 4h
dividee :: [Float] -> [Float]
dividee x = ((head x + x!!1 + x!!2 + x!!3) / 4) :
            [((x!!4 + x!!5 + x!!6 + x!!7) / 4)]

str_to_flt :: [String] -> [Float]
str_to_flt =  map (\y -> read y :: Float)

-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f = map $ map f

-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s : map f m

--i mean..no need to explain :P
get_sleep_total :: Row -> Row
get_sleep_total r = head r : [make_sum r]

--fac suma elementelor si le printez cu 2 zecimale
make_sum :: [String] -> String
make_sum x = fprint $ sum $ list_convert x

--imi face lista din una de stringuri in una de floaturi
list_convert :: [String] -> [Float]
list_convert x = map strflt $ tail x


{-
    TASK SET 2
-}

-- Task 1

tsort :: ColumnName -> Table -> Table
tsort column table = (head table) : whatSort (pos column (head table)) table

whatSort :: Int -> Table -> Table
whatSort i t
    | isInt i (aux t) /= Nothing = sort_int i t
    | isFloat i (aux t) /= Nothing = sort_float i t
    | otherwise = sort_string i t

--iau al 2-lea rand si vad dupa ce tip de date sortez    
aux :: Table -> Row
aux t = head $ tail t

isInt :: Int -> Row -> Maybe Int
isInt i list = readMaybe $ list!!i

isFloat :: Int -> Row -> Maybe Float
isFloat i list = readMaybe $ list!!i

--caut pozitia elemntului in lista
pos :: String -> Row -> Int
pos name [] = 0
pos name list | (head list) /= name = 1 + pos name (tail list)
              | otherwise = 0

--cele 3 functii de mai jos, primesc ca parametru un indice si
--o lista de int, string sau float. si sortarea se face pe elementul
--de pe pozitia i
sort_string :: Int -> Table -> Table
sort_string i x = sortBy op (tail x)
                where op s1 s2
                            | s1!!i < s2!!i = LT
                            | s1!!i > s2!!i  = GT
                            | otherwise = compare (head s1) (head s2)

sort_float :: Int -> Table -> Table
sort_float i x = sortBy op (tail x)
                where op s1 s2
                            | strflt (s1!!i) < strflt (s2!!i) = LT
                            | strflt (s1!!i) > strflt (s2!!i)  = GT
                            | otherwise = compare (head s1) (head s2)

sort_int :: Int -> Table-> Table
sort_int i x = sortBy op (tail x)
                where op s1 s2
                            | str_int (s1!!i) < str_int (s2!!i) = LT
                            | str_int (s1!!i) > str_int (s2!!i) = GT
                            | otherwise = compare (head s1) (head s2)


-- Task 2

vunion :: Table -> Table -> Table
vunion = cmp

unirea :: Table -> Table -> Table
unirea = foldr (:)
--vad daca cele 2 tabele au acelasi header
cmp :: Table -> Table -> Table
cmp t1 t2
    | head t1 == head t2 = unirea (tail t2) t1
    | otherwise = [[]]


-- Task 3

hunion :: Table -> Table -> Table
hunion t1 t2
    | length t1 > length t2 = transpose_union  (extend_table t2 (rest t1 t2)) t1
    | length t1 < length t2 = transpose_union t2 (extend_table t1 (rest t2 t1)) 
    | otherwise = transpose_union t1 t2
   

--imi uneste cele 2 matrice ca in cerinta
transpose_union :: Table -> Table -> Table
transpose_union [] [] = [] 
transpose_union t1 t2 =  foldr (:) (head t1) (head t2) : transpose_union (tail t1) (tail t2)

extend_table :: Table -> Int -> Table
extend_table t no_rows = unirea (ghilimele_table  no_rows (how_many t)) t

ghilimele_table :: Int -> Int -> Table
ghilimele_table no_rows no_el 
    | no_rows > 1 = extend_row no_el : ghilimele_table (no_rows - 1) no_el
    | otherwise =   [extend_row no_el]


--vad cate randuri trb sa adun
rest :: Table -> Table -> Int
rest t1 t2 = length t1 - length t2
--vad cate elemente sunt 
how_many :: Table -> Int
how_many t = length $ head t
--imi completez matricea cu spatii goale sa pot sa le dau merge
extend_row :: Int ->  Row
extend_row i
    | i > 1 = "" : extend_row (i - 1)
    | otherwise = [""]

-- Task 4

tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = t1

-- Task 5

cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = new_column_names : row_f new_row_function (tail t1)  (tail t2)

row_f :: (Row -> Row -> Row) -> Table -> Table -> Table
row_f f t1 [] = []
row_f f [] t2 = []
row_f f t1 t2 = map (f (head t1)) t2 ++ row_f f(tail t1) t2



-- Task 6

projection :: [ColumnName] -> Table -> Table
projection columns_to_extract t = transpose $ map (extract_pos t) columns_to_extract
--aplic functia extract pos pe toate elementele din vectorul de nume

--vad la ce pozitie este numele si extrag elementele de pe pozitia aia
extract_pos :: Table -> String  -> Row
extract_pos t name = extract (pos name (head t)) t

--imi extrage o coloana din tabela
extract :: Int -> Table -> Row
extract pos t = map (!!pos) t

-- Task 7

filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = (head t) : filter(\s -> condition (s !! pos key_column (head t))) (tail t)
--fac filter pe toata tabela si vad daca el de la poz ColumnName indeplineste conditia
