module Lib
    ( Weather(..)
    , averageTemp
    , filterByTemp
    , maxPrecipitationDay
    , minTempDay
    , maxTempDay
    , totalPrecipitation
    -- Новые функции для работы с файлами:
    , saveWeatherToFile
    , loadWeatherFromFile
    ) where

import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe, fromMaybe)
import Text.Read (readMaybe)
import System.IO (withFile, IOMode(ReadMode, WriteMode), hPutStrLn)
import Data.List (intercalate)

-- Тип данных для записи о погоде
data Weather = Weather
    { date          :: String  -- Дата в формате "ГГГГ-ММ-ДД"
    , tempMax       :: Double  -- Максимальная температура
    , tempMin       :: Double  -- Минимальная температура
    , precipitation :: Double  -- Осадки (мм)
    , humidity      :: Double  -- Влажность (%)
    , wind          :: Double  -- Скорость ветра (м/с)
    } deriving (Show, Eq)

-- 1. Средняя температура за период
-- Рассчитывается как среднее арифметическое от средних температур каждого дня
averageTemp :: [Weather] -> Double
averageTemp [] = 0.0
averageTemp records =
    let dailyAverages = [ (tempMax w + tempMin w) / 2 | w <- records ]
    in sum dailyAverages / fromIntegral (length records)

-- 2. Фильтрация записей по порогу средней температуры
filterByTemp :: Double -> [Weather] -> [Weather]
filterByTemp threshold records =
    filter (\w -> (tempMax w + tempMin w) / 2 > threshold) records

-- 3. День с максимальным количеством осадков
maxPrecipitationDay :: [Weather] -> Maybe Weather
maxPrecipitationDay [] = Nothing
maxPrecipitationDay records = Just $ maximumBy (comparing precipitation) records

-- 4. День с минимальной температурой (по tempMin)
minTempDay :: [Weather] -> Maybe Weather
minTempDay [] = Nothing
minTempDay records = Just $ minimumBy (comparing tempMin) records

-- 5. День с максимальной температурой (по tempMax)
maxTempDay :: [Weather] -> Maybe Weather
maxTempDay [] = Nothing
maxTempDay records = Just $ maximumBy (comparing tempMax) records

-- 6. Суммарное количество осадков за период
totalPrecipitation :: [Weather] -> Double
totalPrecipitation records = sum [ precipitation w | w <- records ]

-- Сохранить список записей в файл (CSV-формат)
saveWeatherToFile :: FilePath -> [Weather] -> IO ()
saveWeatherToFile path records = withFile path WriteMode $ \handle -> do
    mapM_ (hPutStrLn handle . weatherToCSV) records

-- Загрузить записи из файла (CSV-формат)
loadWeatherFromFile :: FilePath -> IO [Weather]
loadWeatherFromFile path = do
    contents <- readFile path
    let lines' = lines contents
    return $ mapMaybe parseWeatherLine lines'

-- Преобразовать Weather в строку CSV
weatherToCSV :: Weather -> String
weatherToCSV w = intercalate "," 
    [ date w
    , show (tempMax w)
    , show (tempMin w)
    , show (precipitation w)
    , show (humidity w)
    , show (wind w)
    ]

-- Разобрать одну строку CSV в Weather (с обработкой ошибок)
parseWeatherLine :: String -> Maybe Weather
parseWeatherLine line = case splitOn ',' line of
    [d, tMax, tMin, prec, hum, w] -> do
        tMax' <- readMaybe tMax
        tMin' <- readMaybe tMin
        prec' <- readMaybe prec
        hum'  <- readMaybe hum
        w'    <- readMaybe w
        Just $ Weather d tMax' tMin' prec' hum' w'
    _ -> Nothing

-- Вспомогательная функция: разбить строку по разделителю
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c s = case break (== c) s of
    (x, [])     -> [x]
    (x, _:xs)   -> x : splitOn c xs