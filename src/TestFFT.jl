using HistData, CollUtil, DrawUtil, FFTW
retMakeSeq2(xs) = mapRoll2(xs) do x1, x2; 100*(x2/x1 - 1.0) end

# sampling rate per period
sampleRate = 100.0

sindat = [sin(2 * pi * x) for x in 0.0:0.01:10.0]

freqXs = fftshift(fftfreq(length(sindat), sampleRate))
freqYs = sindat |> fft |> fftshift .|> abs
drawBars(freqXs, freqYs)

# ============

sampleRate = 100
rets = rand(1000)

freqXs = fftshift(fftfreq(length(rets), sampleRate))
freqYs = rets |> fft |> fftshift .|> abs
drawBars(freqXs, freqYs)

# ============

sampleRate = 1000
dailys = dataDaily()
closes = [d.close for d in dailys]
rets = retMakeSeq2(closes)

freqXs = fftshift(fftfreq(length(rets), sampleRate))
freqYs = rets |> fft |> fftshift .|> abs
drawBars(freqXs, freqYs)
