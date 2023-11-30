module FilesJLD2
using JLD2
import DateUtil, Paths

function Paths.save_data(path; kws...)
    !isempty(kws) || throw(ArgumentError("FilesJLD2 save_data requires keyword arguments"))
    mkpath(dirname(path))
    JLD2.jldsave(path; kws...)
end

function Paths.load_data(path, names...; age=DateUtil.FOREVER2, asof=DateUtil.DATETIME_BEFORE)
    !isempty(names) || throw(ArgumentError("FilesJLD2 load_data requires names"))
    Paths.check_file_mtime(path, age, asof)
    return JLD2.load(path, names...)
end

end