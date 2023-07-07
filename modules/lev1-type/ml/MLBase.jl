module MLBase
using Intervals

const DateRange = Ref(Interval{Closed,Open}(Date(2020,2,1), Date(2020,3,1)))

end
