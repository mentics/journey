module Todo
using Dates
using SH, OptionTypes
using CaptUtil, Emails
using Globals, Store, Markets
using CmdUtil

# ("check-Todo", @__MODULE__, "checkTodo", "checkTodoWhen", true),

# checkTodoWhen(from::DateTime, isMktOpen::Bool, nextMktChange::DateTime) = ( t1 = marketClose(expir(0)) - Minute(37) ; from < t1 ? t1 : marketClose(expir(1)) - Minute(37) )
function checkTodo()
    report = IOBuffer()
    for t in tradesToClose(0)
        mustClose = filter(getLegs(t)) do leg
            isNitm(getOption(leg), market().curp, Globals.get(:Strats))
        end
        print(report, getId(t), ": ")
        isempty(mustClose) ? println(report, "none") : println(report, getId.(mustClose))
    end
    println(report) ; println(report)
    println(report, captureStdio(dbChecks))
    sendEmail("## REMOVED EMAIL ##", "Close positions $(today())", String(take!(report)))
end

end
