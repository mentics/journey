module DataOptions

format_ym(year, month) = "$(year)-$(lpad(month, 2, '0'))"
file_quotes(year, month; sym="SPY") = joinpath(dir_incoming("quotes"; sym), "quotes-$(sym)-$(format_ym(year, month)).arrow")


end