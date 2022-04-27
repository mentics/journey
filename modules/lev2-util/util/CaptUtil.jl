module CaptUtil

export captureStdio

function captureStdio(f)
    origout = stdout
    origerr = stderr
    out, outw = redirect_stdout()
    err, errw = redirect_stderr()
    outRead = @async read(out, String)
    errRead = @async read(err, String)
    try
        f()
    finally
        redirect_stdio(;stdout=origout, stderr=origerr)
        close(outw)
        close(errw)
    end
    return fetch(outRead) * "\n\n" * fetch(errRead)
end

end