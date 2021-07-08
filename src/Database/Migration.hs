module Database.Migration where

import Database.Connection (runDBIO)
import Database.Persist.Postgresql (runMigration)
import Model.Image (migrateAll)

-- Note: Code below will need to be uncommented to run migrations.
--       Then run `stack runghc -- src/Script/Migration.hs`.
-- main :: IO ()
-- main = runDBIO $ runMigration migrateAll
