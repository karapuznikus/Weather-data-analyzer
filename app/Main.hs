module Main where

import Lib
import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

-- Основная функция
main :: IO ()
main = do
    putStrLn "=== Анализ погодных данных ==="
    putStrLn "Сначала загрузите данные из файла (пункт 5 в меню)."
    mainLoop []  -- Начинаем с пустого списка

-- Главный цикл программы
mainLoop :: [Weather] -> IO ()
mainLoop records = do
    showMenu
    putStr "Введите номер действия: "
    hFlush stdout -- Принудительно сбрасываем буфер, чтобы текст появился до ввода
    choice <- getLine

    case choice of
        "1" -> do
            let avg = averageTemp records
            putStrLn $ "Средняя температура: " ++ show avg ++ "°C"
            mainLoop records

        "2" -> do
            putStrLn "Дни с экстремальной температурой:"
            case minTempDay records of
                Nothing -> putStrLn "Нет данных."
                Just day -> putStrLn $ "Минимальная температура: " ++ show day
            case maxTempDay records of
                Nothing -> putStrLn "Нет данных."
                Just day -> putStrLn $ "Максимальная температура: " ++ show day
            mainLoop records

        "3" -> do
            putStrLn "День с максимальными осадками:"
            case maxPrecipitationDay records of
                Nothing -> putStrLn "Нет данных."
                Just day -> print day
            mainLoop records

        "4" -> do
            putStr "Введите порог средней температуры для фильтрации: "
            hFlush stdout
            input <- getLine
            case readMaybe input of
                Nothing -> putStrLn "Ошибка: введите число (например, 20.5)."
                Just threshold -> do
                    let filtered = filterByTemp threshold records
                    if null filtered
                        then putStrLn "Нет записей, удовлетворяющих условию."
                        else do
                            putStrLn "Найденные записи:"
                            mapM_ print filtered
            mainLoop records

        "5" -> do
            putStr "Введите имя файла для загрузки (например, weather.csv): "
            hFlush stdout
            filename <- getLine
            records <- loadWeatherFromFile filename
            if null records
                then putStrLn "Ошибка: файл не найден или содержит некорректные данные."
                else putStrLn $ "Загружено " ++ show (length records) ++ " записей."
            mainLoop records

        "6" -> do
            putStr "Введите имя файла для сохранения (например, result.csv): "
            hFlush stdout
            filename <- getLine
            saveWeatherToFile filename records
            putStrLn $ "Данные сохранены в файл '" ++ filename ++ "'."
            mainLoop records

        "0" -> putStrLn "До свидания!"

        _ -> do
            putStrLn "Неверный выбор. Попробуйте снова."
            mainLoop records

-- Меню
showMenu :: IO ()
showMenu = putStrLn $ unlines
    [ "=== Меню ==="
    , "1 - Подсчитать среднюю температуру"
    , "2 - Найти дни с экстремальной температурой"
    , "3 - Найти дни с максимальными осадками"
    , "4 - Фильтровать записи по температуре"
    , "5 - Загрузить данные из файла"
    , "6 - Сохранить результаты в файл"
    , "0 - Выход"
    , "============"
    ]