module CmdSched
using Dates
using CollUtil, DateUtil, ThreadUtil, Emails
using Sched, Calendars

export notif, nonotif

import Markets:market
function notif(low::Real, high::Real)
    notif("bracket") do
        !(low < market().curp < high) ? "Market out of bracket $(low) - $(high): $(market().curp)" : nothing
    end
end

function notif(pred::Function, name::String)
    Notifs[name] = Notif(name, pred)
    Sched.add("notif", @__MODULE__, "check", "whenCheck", true; repok=true)
end

function nonotif(name::String)
    runSync(Lock) do
        delete!(Notifs, name)
        if isempty(Notifs)
            Sched.remove("notif")
        end
        # del!(Notifs) do n
        #     n.name == name
        # end
    end
end

#region Local
const Lock = ReentrantLock()
struct Notif
    name::String
    pred::Function
end

Notifs = Dict{String,Notif}()

whenCheck(from::DateTime, isMktOpen::Bool, tsMktChange::DateTime) = nextMarketPeriod(from, isMktOpen, tsMktChange, Second(30), Second(1), Second(1))
function check()
    runSync(Lock) do
        for (name, n) in Notifs
            msg = n.pred()
            if !isnothing(msg)
                sendEmail("## REMOVED EMAIL ##", name, msg)
            end
        end
    end
end
#endregion

end
