module CmdTools
using BaseTypes
using OutputUtil
using Expirations

export exprs
exprs() = pretyble([(;expir=x) for x in expirs()]; rowcol=true)

end