module SqlLoader
using CSV, Tables, SQLite
import Sqlite as sql

function loadCsv(db, csvPath, tableName; checkTypeRows=100, dropCols=[])
    rows = CSV.File(csvPath; rows_to_check=checkTypeRows, limit=checkTypeRows) # downcast=true
    rows = CSV.Rows(csvPath; types=collect(Tables.schema(rows).types), drop=dropCols, reusebuffer=true)
    SQLite.load!(rows, db, tableName; analyze=true)
    cnt = first(sql.run(db, "select count(*) as cnt from " * tableName)).cnt
    return cnt
end

end