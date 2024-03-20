module Emails
using SMTPClient, Dates, TimeZones
using BaseTypes

export sendEmail

function sendEmail(to::AStr, subject::AStr, body::AStr)
    opt = SendOptions(isSSL = true, username = "## REMOVED EMAIL ##", passwd = "## REMOVED PASSWORD ##")
    dateStr = Dates.format(now(tz"UTC"), "eee, dd uuu yyyy HH:MM:SS +0000")
    #Provide the message body as RFC5322 within an IO
    body = IOBuffer(
            "Date: $(dateStr)\r\n" *
            "From: <## REMOVED EMAIL ##>\r\n" *
            "To: $(to)\r\n" *
            "Subject: $(subject)\r\n" *
            "\r\n" *
            "$(body)\r\n")
    url = "smtp://smtp.gmail.com:587" # "smtp://smtp.gmail.com:465"
    rcpt = ["<$(to)>"]
    from = "## REMOVED EMAIL ##"
    resp = SMTPClient.send(url, rcpt, from, body, opt)
    # @info "Email response" resp
end

end
