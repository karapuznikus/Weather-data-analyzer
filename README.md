# Weather Data Analyzer (Haskell)

## Description
A console application for analyzing weather data. Users can load weather records from a CSV file, calculate statistics (average temperature, extreme days), filter data, and save results. Built with pure functional programming in Haskell.

## Requirements (Windows)

- **Haskell Toolchain**: GHC (Glasgow Haskell Compiler) and Stack
- **Operating System**: Windows 10 or 11
- **No additional libraries required**

### How to Install Haskell (if not installed)

1. Go to [https://www.haskell.org/ghcup/](https://www.haskell.org/ghcup/)
2. Download and run the **Windows installer** (`ghcup-install.exe`)
3. Follow the installation steps (keep default options)
4. After installation, **restart your terminal or VS Code**
5. Verify installation:
   ```cmd
   ghc --version
   stack --version
   ```

### How to Run

1. Open Command Prompt `cmd` in the project folder and run:
        ```cmd
        stack build
        ```
2. Run the application:
        ```cmd
        stack run
        ```
3. Use the program:
    First, load data using menu option `5`.
    The program expects a `CSV` file with following format (no header): `date,tempMax,tempMin,precipitation,humidity,wind`
    for example (weather.csv):
        ```csv
        2024-06-01,25.0,15.0,0.0,60.0,5.0
        2024-06-02,28.0,18.0,2.5,70.0,8.0
        ```

### Menu options

1: Calculates the average daily temperature over the loaded period.
2: Shows the days with the lowest and highest temperatures.
3: Finds the day with the most precipitation.
4: Filters records where the average temperature is above a user-defined threshold.
5: Loads weather data from a CSV file (e.g., `weather.csv`).
6: Saves the currently loaded data to a new CSV file.

#### Notes 

If you see garbled text, run `chcp 65001` before `stack run` to enable UTF-8:
    ```cmd
    chcp 65001
    stack run
    ```