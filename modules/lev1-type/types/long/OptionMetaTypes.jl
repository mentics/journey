module OptionMetaTypes
using SH

export OptionMeta

struct OptionMeta
    iv::Float64
end
OptionMeta(;iv=0.18) = OptionMeta(iv)
SH.getIv(m::OptionMeta) = m.iv

end