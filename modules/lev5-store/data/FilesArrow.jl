module FilesArrow
using Arrow
import DataFrames:AbstractDataFrame,DataFrame
import DateUtil, Paths

function Paths.save_data(path, df::AbstractDataFrame; update=false)
    mkpath(dirname(path))
    if update
        mv(path, tempname())
    end
    Arrow.write(path, df)
end

function Paths.load_data(path, ::Type{DataFrame}; age=DateUtil.FOREVER2, asof=DateUtil.DATETIME_BEFORE, copycols=false)
    Paths.check_file_mtime(path, age, asof)
    return DataFrame(Arrow.Table(path); copycols)
end

end