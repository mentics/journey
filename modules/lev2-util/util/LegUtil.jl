module LegUtil
using SH, BaseTypes
using ChainTypes, LegQuoteTypes
using OptionUtil

export extrinsDir

extrinsDir(lm::LegQuote, curp) = calcExtrins(to(OptionQuote, lm), curp)[3] * getQuantityDir(getLeg(lm))
extrinsDir(lms::Coll{LegQuote}, curp) = sum(lm -> extrinsDir(lm, curp), lms)

end