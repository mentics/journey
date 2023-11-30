module FilesArrow
using Arrow
import DataFrames:AbstractDataFrame,DataFrame
import DateUtil, Paths

function Paths.save_data(path, df::AbstractDataFrame)
    mkpath(dirname(path))
    Arrow.write(path, df)
end

function Paths.load_data(path, ::Type{DataFrame}; age=DateUtil.FOREVER2, asof=DateUtil.DATETIME_BEFORE)
    Paths.check_file_mtime(path, age, asof)
    return DataFrame(Arrow.Table(path); copycols=false)
end

end