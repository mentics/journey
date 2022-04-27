module DrawChains
using GLMakie
using SH, SmallTypes, DrawUtil
using Chains, Expirations

export drawIv

drawIv() = draw(getIv.(getMeta.(chains()[expir(1)].chain)))
drawIv(style::Style.T) = draw(getIv.(getMeta.(filter(x->getStyle(x)==style,chains()[expir(1)].chain))))

end