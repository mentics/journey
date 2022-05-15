module SystemUtil

# to call win api from julia: https://stackoverflow.com/questions/30555994/calling-win32-functions-from-julia
# https://docs.julialang.org/en/v1/manual/calling-c-and-fortran-code/
# https://www.prowaretech.com/articles/current/windows/application-programming-interface/prevent-computer-going-to-sleep

# https://docs.microsoft.com/en-us/windows/win32/power/system-wake-up-events
# TODO: CreateWaitableTimer might make it easy to schedule times to wake up and so wouldn't need to use task scheduler

# TODO: schedule win task scheduler so we can tell it to go to sleep and wake up again after some time
# TODO: can we check idle timer to see if user is currently active so we don't sleep while someone using it?

export suspendNow, setAllowSuspend

# https://docs.microsoft.com/en-us/windows/win32/api/powrprof/nf-powrprof-setsuspendstate
suspendNow() = ccall((:SetSuspendState, "PowrProf"), Cint, (Cint,Cint,Cint), false, false, false)

# https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-setthreadexecutionstate
setAllowSuspend(allow::Bool) = ccall((:SetThreadExecutionState, "Kernel32"), Culonglong, (Culonglong,),
        allow ? ES_CONTINUOUS : ES_CONTINUOUS | ES_SYSTEM_REQUIRED | ES_AWAYMODE_REQUIRED)

using Dates
using LogUtil
const WakeTimer = Ref{Timer}()
const LastRun = Ref{DateTime}(now(UTC))
const Listeners = Vector{Function}()
WakeTimerPeriod = 10
function callOnWake(callback::Function)::Nothing
    if !isassigned(WakeTimer)
        LastRun[] = now(UTC)
        WakeTimer[] = Timer(onTimer, WakeTimerPeriod; interval=WakeTimerPeriod)
    end
    isopen(WakeTimer[]) || @logerr "WakeTimer not running"
    push!(Listeners, callback)
    return
end
function onTimer(::Timer)
    nn = now(UTC)
    if LastRun[] < nn - Second(2 * WakeTimerPeriod)
        for lister in Listeners
            lister()
        end
    end
    LastRun[] = nn
end

# function registerWakeCallback(f)
#     PBT_APMRESUMEAUTOMATIC =
#     HANDLE = Ptr{Void}
#     LPARAM = Clong
#     LPCGUID =
#     DWORD = Cuint
#     function localCallback(hwnd::HANDLE, lParam::LPARAM)
#         println
#     end
#     const cback = cfunction(localCallback, Cint, (HANDLE, LPARAM))

#     ccall((:RegisterPowerSettingNotification, "User32"), HANDLE, (HANDLE, LPCGUID, DWORD), cback, 0)
# end

#region Local
const ES_AWAYMODE_REQUIRED = 0x00000040
const ES_CONTINUOUS = 0x80000000
const ES_DISPLAY_REQUIRED = 0x00000002
const ES_SYSTEM_REQUIRED = 0x00000001
const ES_USER_PRESENT = 0x00000004
#endregion

end