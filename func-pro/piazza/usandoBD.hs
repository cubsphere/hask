-- Conectando
{-
> :module Database.HDBC Database.HDBC.Sqlite3
> conn <- connectSqlite3 "test1.db"
> :type conn

> disconnect conn

-}

-- Queries

-- Funcao "run" envia queries para uma base de dados

{-
conn <- connectSqlite3 "test1.db"
run conn "CREATE TABLE test (id INTEGER NOT NULL, desc VARCHAR(80))" []
run conn "INSERT INTO test (id) VALUES (0)" []

commit conn
disconnect conn
-}

-- Parametros de queries

{-
conn <- connectSqlite3 "test1.db"
run conn "INSERT INTO test VALUES (?, ?)" [toSql 0, toSql "zero"]
commit conn
disconnect conn

-}

-- Comandos
{-
conn <- connectSqlite3 "test1.db"
stmt <- prepare conn "INSERT INTO test VALUES (?, ?)"
execute stmt [toSql (1::Int), toSql "one"]
execute stmt [toSql (2::Int), toSql "two"]
execute stmt [toSql (3::Int), toSql "three"]
execute stmt [toSql 4, SqlNull]
commit conn
disconnect conn

-- Executando varios comandos
conn <- connectSqlite3 "test1.db"
stmt <- prepare conn "INSERT INTO test VALUES (?, ?)"
executeMany stmt [[toSql 5, toSql "five's nice"], [toSql 6, SqlNull]]
commit conn
disconnect conn

-}


-- Lendo resultados
{-
conn <- connectSqlite3 "test1.db"
quickQuery' conn "SELECT * from test where id < 2" []
disconnect conn
-}



{-
conn <- connectSqlite3 "test1.db"
stmt <- prepare conn "SELECT * from test where id < 2"
execute stmt []

results <- fetchAllRowsAL stmt
mapM_ print results
> disconnect conn
-}

--
{-
-}

import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC

{- | Define a function that takes an integer representing the maximum
id value to look up.  Will fetch all matching rows from the test database
and print them to the screen in a friendly format. -}
query :: Int -> IO ()
query maxId = 
    do -- Connect to the database
       conn <- connectSqlite3 "test1.db"

       -- Run the query and store the results in r
       r <- quickQuery' conn
            "SELECT id, desc from test where id <= ? ORDER BY id, desc"
            [toSql maxId]

       -- Convert each row into a String
       let stringRows = map convRow r
                        
       -- Print the rows out
       mapM_ putStrLn stringRows

       -- And disconnect from the database
       disconnect conn

    where convRow :: [SqlValue] -> String
          convRow [sqlId, sqlDesc] = 
              show intid ++ ": " ++ desc
              where intid = (fromSql sqlId)::Integer
                    desc = case fromSql sqlDesc of
                             Just x -> x
                             Nothing -> "NULL"
          convRow x = fail $ "Unexpected result: " ++ show x

