module MLBase
using Intervals

const DateRange = Ref(Interval{Closed,Open}(Date(2020,2,1), Date(2020,7,1)))
const DateRangeExt = Ref(Interval{Closed,Open}(Date(2019,11,1), Date(2020,11,30)))

end