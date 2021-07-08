module Script.Migration where

import Database.Connection (runDBIO)
import Database.Persist.Postgresql (runMigration)
import Model.Image (migrateAll)

main :: IO ()
main = runDBIO $ runMigration migrateAll
