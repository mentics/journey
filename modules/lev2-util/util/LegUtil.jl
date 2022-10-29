module LegUtil
using SH, BaseTypes
using ChainTypes, LegMetaTypes
using OptionUtil

export extrinsDir

extrinsDir(lm::LegMeta, curp) = calcExtrins(to(OptionQuote, lm), curp)[3] * getQuantityDir(getLeg(lm))
extrinsDir(lms::Coll{LegMeta}, curp) = sum(lm -> extrinsDir(lm, curp), lms)

end