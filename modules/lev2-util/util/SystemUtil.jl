module SystemUtil

# to call win api from julia: https://stackoverflow.com/questions/30555994/calling-win32-functions-from-julia
# https://docs.julialang.org/en/v1/manual/calling-c-and-fortran-code/
# https://www.prowaretech.com/articles/current/windows/application-programming-interface/prevent-computer-going-to-sleep

# TODO: schedule win task scheduler so we can tell it to go to sleep and wake up again after some time
# TODO: can we check idle timer to see if user is currently active so we don't sleep while someone using it?

export suspendNow, setAllowSuspend

# https://docs.microsoft.com/en-us/windows/win32/api/powrprof/nf-powrprof-setsuspendstate
suspendNow() = ccall((:SetSuspendState, "PowrProf"), Cint, (Cint,Cint,Cint), false, false, false)

# https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-setthreadexecutionstate
setAllowSuspend(allow::Bool) = ccall((:SetThreadExecutionState, "Kernel32"), Culonglong, (Culonglong,),
        allow ? ES_CONTINUOUS : ES_CONTINUOUS | ES_SYSTEM_REQUIRED | ES_AWAYMODE_REQUIRED)

#region Local
const ES_AWAYMODE_REQUIRED = 0x00000040
const ES_CONTINUOUS = 0x80000000
const ES_DISPLAY_REQUIRED = 0x00000002
const ES_SYSTEM_REQUIRED = 0x00000001
const ES_USER_PRESENT = 0x00000004
#endregion

end