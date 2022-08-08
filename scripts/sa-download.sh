#!/bin/bash


curl 'https://seekingalpha.com/api/v3/screener_results' \
  -H 'authority: seekingalpha.com' \
  -H 'accept: */*' \
  -H 'accept-language: en-US,en;q=0.9' \
  -H 'content-type: application/json' \
  -H 'cookie: machine_cookie=6071666182064; _igt=eac4b6a3-4f66-497e-db45-26a99710806c; _cls_s=364f8035-f84b-4d81-b13e-0563633b4c05:0; _cls_v=6b22da5b-8cd3-4529-be8d-ea1bf8a5aa9d; __pat=-14400000; _ga=GA1.2.303343256.1658620965; _gcl_au=1.1.770029613.1658620965; _fbp=fb.1.1658620964957.187023299; _cc_id=bbdbd0ec9054d648845a516a9034d5e1; prism_25946650=7ea4c454-399e-4576-b169-279f902e84b8; _pxvid=f1e5bfaa-0ae3-11ed-96d7-5749446d4366; pxcts=f1e5cbb2-0ae3-11ed-96d7-5749446d4366; ga_clientid=303343256.1658620965; g_state={"i_l":0}; user_id=54607428; u_voc=; marketplace_author_slugs=; user_perm=; user_remember_token=d5e1f67e796f682b3e605bf4283e8c33443cdb68; _gac_UA-142576245-3=1.1658695017.Cj0KCQjw2_OWBhDqARIsAAUNTTFT5kgCIX7TbzJHIG1_swVZT2ehZO6tRDKsqB5b6_hnYylaECFxcv0aAgfNEALw_wcB; sailthru_hid=e865f74ef53b891c8276d027693f33ac60e4d2352e08d924735ce6b556c6bc3e5d4403affe742307ff74a234; __pnahc=0; has_paid_subscription=true; ever_pro=1; sapu=12; user_nick=taotree; _hjSessionUser_65666=eyJpZCI6ImM5Nzg3ZGYwLTRlODEtNWUxOS1iY2UwLTcxYjI0N2UyZDA5MCIsImNyZWF0ZWQiOjE2NTkxOTA0OTE2OTMsImV4aXN0aW5nIjp0cnVlfQ==; user_devices=2; __tae=1659754665990; __tac=; _gid=GA1.2.1902944425.1659875734; panoramaId=ef49906a77f3dcf67f1752487ebc16d53938221439d41fa5de9ff54dc80f7bdb; panoramaId_expiry=1660480534033; _gcl_aw=GCL.1659879239.Cj0KCQjwxb2XBhDBARIsAOjDZ36qEUfFUGzvYR1l_j87V_kwCJhrSVi-d_St16yGuxzQuVhEadbGtZkaAp9cEALw_wcB; _gac_UA-142576245-1=1.1659879239.Cj0KCQjwxb2XBhDBARIsAOjDZ36qEUfFUGzvYR1l_j87V_kwCJhrSVi-d_St16yGuxzQuVhEadbGtZkaAp9cEALw_wcB; _hjCachedUserAttributes=eyJhdHRyaWJ1dGVzIjp7ImxvZ2dlZF9pbiI6dHJ1ZSwibXBfc3ViIjpmYWxzZSwicHJlbWl1bV9zdWIiOnRydWUsInByb19zdWIiOmZhbHNlfSwidXNlcklkIjoiNTQ2MDc0MjgifQ==; _gac_UA-142576245-4=1.1659879240.Cj0KCQjwxb2XBhDBARIsAOjDZ36qEUfFUGzvYR1l_j87V_kwCJhrSVi-d_St16yGuxzQuVhEadbGtZkaAp9cEALw_wcB; _ig=54607428; _clck=g8tqn|1|f3u|0; session_id=e55e6428-7812-4585-a2d8-8eca993ec86b; _sasource=; _uetsid=6e456600164d11eda6108b3e96e4d981; _uetvid=9aa4ca703e5f11ec8dfc43f19f496f0e; sailthru_visitor=108bebf1-2815-489a-8d93-bf931b716ad9; sailthru_content=4fbb613834ea8cc57feae8bd1e4aaeae4dfcd4abf2c20d3c6d398b4f5a8270b3a6868281678e0269f2e30c72a59520f86ee54343d85ccfb04f65bd847aaafeb5ed016a510cc06d5e79dc1db8608d729a0cf0526f52035a3a6aa562a10b4bae2be1980fd747e58a39e7568037500e1f926a6ebc798cc2c232c5ef8b94521c981527196a8a2587992b557d4b875d9a736c05bccff52c907303182a1f2e536426349dc8a9254a0258aae32f96507bf440950df6122f6a027444bc544aeb909fceb68779dc62b8018008a97c994a984f1063f2bf59f907da11ef259c5d53c8a99e070e50ea7ed4f2934b0685f4b9504786cec6c6ef74477fc2a27e5e8ab2a51659a3; _clsk=w2xq1y|1659954537422|6|0|i.clarity.ms/collect; _pxde=317e00236949e85f15251435a86822b62b0dd6c7d8664cfe6bde2db3292d9508:eyJ0aW1lc3RhbXAiOjE2NTk5NTQ1NzEzNzgsImZfa2IiOjB9; _px=hqOop1qkPRlTfmZleXEIa1AscSRNQgqTULokU5ReM6yd9wI9/IeabT7ULQ213Dk+kjSs4MrA76R7uWj5eJoMog==:1000:qmceoYZ5JNenu68ApKvuHG4PI8glUe+m3lzGHgq23qN4bjppPo/ZiolI9oyt/RpchpO8wHFO6g0eI7FGmaoKtb/zsGj3v6JneZ5XPUYK8ive/tbckGQgfB2dVPvu+tkLnPGwASRtGysjs8hz0NShpTs0ap8V0O1cUDKh/hF9kreNc/69aktNRXmGAlHVhQM9cXdAXQdJG0b3i99FDXnYi8GrAK/SIjbWh1iMYfIG6VsNjVYehoF1zC9vy7kXD1Dh9fdPXPMwm4y1LNcG+Ep1KQ==; _px2=eyJ1IjoiMTUxMGZkYTAtMTZmZi0xMWVkLWI3NTktMDVlY2VmMTgxMTg4IiwidiI6ImYxZTViZmFhLTBhZTMtMTFlZC05NmQ3LTU3NDk0NDZkNDM2NiIsInQiOjE2NTk5NTUwNzEzNzgsImgiOiIwMjgyNGNhNWEyZTRjZjE1MTYxYTE1ZTQzMGVkNDUwMGRjNzhiNWU5N2UxMTQzZWEzNGNhNmZmM2M3NzUyNjA2In0=; _gat_UA-142576245-4=1; LAST_VISITED_PAGE=%7B%22pathname%22%3A%22https%3A%2F%2Fseekingalpha.com%2Fscreeners%2F922a27ae98-To-Download%2Ftrading%22%2C%22pageKey%22%3A%224f3644c6-087d-4387-b05b-13f44d41cfd9%22%7D; user_cookie_key=1342hsv; gk_user_access=1*archived*1659954608; gk_user_access_sign=bb3cfeda030d4f80cef25db2ff9773762ab2d079; _pctx=%7Bu%7DN4IgDghg5gpgagSxgdwJIBMQC4QBsBsA1gLYAsArPsQHYCcAjAgEYBMAHgI4gA0ITATgHtkAZxj8M2POQCehAMwJSAL1LEADAhEA3dJl4BXMfxHZqB3LkPGAygBcIdo2YtWQIhHZiSs5ywF8gA; __pvi=%7B%22id%22%3A%22v-2022-08-08-03-27-33-202-purfcwFClVfMhaiq-5b443c5e466197036a9c2591726a2aba%22%2C%22domain%22%3A%22.seekingalpha.com%22%2C%22time%22%3A1659954608449%7D; sailthru_pageviews=3; __tbc=%7Bkpex%7DW6oShuI0nLVBtUkcsYyQNO3HbqO1F1NSDqxDOemOp1I2VzJNwbAM-L7VWV3iXEZeONhV6ZY_iID8vDMKo689yG6JVQyV6-Y5zmElMQcneQqhAoGIqkJOLfI05scsegea; xbc=%7Bkpex%7DEUPy8flmGBQ-FhtZwp6XQa8oHL1LXaI6DgepA81zoVYnB9hA2UEAmS9MNnm_cJP-XwsLhsFO4qSUxA8KFcOBItcxy6Hf0o6sDOQWS8nvXPVcrLz8MS9Khp2ijAmxULIbqXfaegWMFM7R8Q4qHI--jHneruSFac3Mm8RTWtEd57IkVzIRtnXYpFQc9b3bh5DqUllag2yx72VFuiW9Jbbgu_SBaI7kWx6aJYRuMNLmvcwo3dxKBLEZA1Z_wbgaKY5kkFz7f0ZQ-OoM4Fj2Kv-0YWnr46alCxCYawVmzw_n1dWL_4dLBqH0dxR5Ih2TxKxCKLjtQvngU7dXY2aXvLppdd4Ecrk14jgEZwFoG0HTLD3wvi97W605_34Mn8kuqBOTBalH1bIHGYm4N2kEkXAKrPqdBuwGaGIcH10pMG428V0hdbrC7vstC5e6CHGD-3CZde0m_-yczW2tj-kA1NQBZxArAURL6R_XQ-9LG9SL3n2dFK3sX-W8tcq8B5qEiqHm; _dc_gtm_UA-142576245-1=1' \
  -H 'origin: https://seekingalpha.com' \
  -H 'referer: https://seekingalpha.com/screeners/922a27ae98-To-Download/trading' \
  -H 'sec-ch-ua: ".Not/A)Brand";v="99", "Google Chrome";v="103", "Chromium";v="103"' \
  -H 'sec-ch-ua-mobile: ?0' \
  -H 'sec-ch-ua-platform: "Windows"' \
  -H 'sec-fetch-dest: empty' \
  -H 'sec-fetch-mode: cors' \
  -H 'sec-fetch-site: same-origin' \
  -H 'user-agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.0.0 Safari/537.36' \
  --data-raw '{"filter":{"close":{"gte":0.01,"disabled":false}},"quant_rank":true,"sort":null,"type":"stock","per_page":100,"page":1}' \
  --compressed


# curl 'https://seekingalpha.com/api/v3/screener_results' \
#   -H 'authority: seekingalpha.com' \
#   -H 'accept: */*' \
#   -H 'accept-language: en-US,en;q=0.9' \
#   -H 'content-type: application/json' \
#   -H 'cookie: machine_cookie=6071666182064; _igt=eac4b6a3-4f66-497e-db45-26a99710806c; _cls_s=364f8035-f84b-4d81-b13e-0563633b4c05:0; _cls_v=6b22da5b-8cd3-4529-be8d-ea1bf8a5aa9d; __pat=-14400000; _ga=GA1.2.303343256.1658620965; _gcl_au=1.1.770029613.1658620965; _fbp=fb.1.1658620964957.187023299; _cc_id=bbdbd0ec9054d648845a516a9034d5e1; prism_25946650=7ea4c454-399e-4576-b169-279f902e84b8; _pxvid=f1e5bfaa-0ae3-11ed-96d7-5749446d4366; pxcts=f1e5cbb2-0ae3-11ed-96d7-5749446d4366; ga_clientid=303343256.1658620965; g_state={"i_l":0}; user_id=54607428; u_voc=; marketplace_author_slugs=; user_perm=; user_remember_token=d5e1f67e796f682b3e605bf4283e8c33443cdb68; _gac_UA-142576245-3=1.1658695017.Cj0KCQjw2_OWBhDqARIsAAUNTTFT5kgCIX7TbzJHIG1_swVZT2ehZO6tRDKsqB5b6_hnYylaECFxcv0aAgfNEALw_wcB; sailthru_hid=e865f74ef53b891c8276d027693f33ac60e4d2352e08d924735ce6b556c6bc3e5d4403affe742307ff74a234; __pnahc=0; has_paid_subscription=true; ever_pro=1; sapu=12; user_nick=taotree; _hjSessionUser_65666=eyJpZCI6ImM5Nzg3ZGYwLTRlODEtNWUxOS1iY2UwLTcxYjI0N2UyZDA5MCIsImNyZWF0ZWQiOjE2NTkxOTA0OTE2OTMsImV4aXN0aW5nIjp0cnVlfQ==; user_devices=2; session_id=922a0694-00c9-4e0b-ac93-f0b66a5457db; __tae=1659754665990; __tac=; _gid=GA1.2.1902944425.1659875734; panoramaId=ef49906a77f3dcf67f1752487ebc16d53938221439d41fa5de9ff54dc80f7bdb; panoramaId_expiry=1660480534033; _gcl_aw=GCL.1659879239.Cj0KCQjwxb2XBhDBARIsAOjDZ36qEUfFUGzvYR1l_j87V_kwCJhrSVi-d_St16yGuxzQuVhEadbGtZkaAp9cEALw_wcB; _gac_UA-142576245-1=1.1659879239.Cj0KCQjwxb2XBhDBARIsAOjDZ36qEUfFUGzvYR1l_j87V_kwCJhrSVi-d_St16yGuxzQuVhEadbGtZkaAp9cEALw_wcB; _hjCachedUserAttributes=eyJhdHRyaWJ1dGVzIjp7ImxvZ2dlZF9pbiI6dHJ1ZSwibXBfc3ViIjpmYWxzZSwicHJlbWl1bV9zdWIiOnRydWUsInByb19zdWIiOmZhbHNlfSwidXNlcklkIjoiNTQ2MDc0MjgifQ==; _gac_UA-142576245-4=1.1659879240.Cj0KCQjwxb2XBhDBARIsAOjDZ36qEUfFUGzvYR1l_j87V_kwCJhrSVi-d_St16yGuxzQuVhEadbGtZkaAp9cEALw_wcB; _ig=54607428; _clck=g8tqn|1|f3u|0; _hjSession_65666=eyJpZCI6IjFhZmY0YjNiLWIxOTMtNDY5My1hYmFjLTFhZTY4ZDRmZWMzZiIsImNyZWF0ZWQiOjE2NTk5MzczMDcxNDIsImluU2FtcGxlIjpmYWxzZX0=; _hjAbsoluteSessionInProgress=0; user_cookie_key=v9808k; gk_user_access=1*archived*1659937712; gk_user_access_sign=b9a9f74c8724dc98d5a6a0354b9a41c6704a913a; LAST_VISITED_PAGE=%7B%22pathname%22%3A%22https%3A%2F%2Fseekingalpha.com%2Fscreeners%2F922a27ae98-To-Download%2Ftrading%22%2C%22pageKey%22%3A%228d71213c-646f-4fbd-9a02-f92ffe40fa7b%22%7D; _pctx=%7Bu%7DN4IgDghg5gpgagSxgdwJIBMQC4QBsBsA1gMYDMAFggM4zEBMADAFYAeALAiADQgBGATgHtkNfhmx4ArAE9CpBGwBebALYNqAN3SYeAV1FVsAO125ce0QGUALhGv7jp8yCoJrMcVhNmAvkA; __pvi=%7B%22id%22%3A%22v-2022-08-07-22-35-05-455-kIliAeii3xMYHqWw-b130a7b8f8ed2b551d9ed77834de6e3b%22%2C%22domain%22%3A%22.seekingalpha.com%22%2C%22time%22%3A1659937781623%7D; __tbc=%7Bkpex%7DW6oShuI0nLVBtUkcsYyQNO3HbqO1F1NSDqxDOemOp1I2VzJNwbAM-L7VWV3iXEZeONhV6ZY_iID8vDMKo689yG6JVQyV6-Y5zmElMQcneQqhAoGIqkJOLfI05scsegea; xbc=%7Bkpex%7DEUPy8flmGBQ-FhtZwp6XQa8oHL1LXaI6DgepA81zoVYnB9hA2UEAmS9MNnm_cJP-XwsLhsFO4qSUxA8KFcOBItcxy6Hf0o6sDOQWS8nvXPVcrLz8MS9Khp2ijAmxULIbqXfaegWMFM7R8Q4qHI--jHneruSFac3Mm8RTWtEd57IkVzIRtnXYpFQc9b3bh5DqUllag2yx72VFuiW9Jbbgu_SBaI7kWx6aJYRuMNLmvcwo3dxKBLEZA1Z_wbgaKY5kkFz7f0ZQ-OoM4Fj2Kv-0YWnr46alCxCYawVmzw_n1dWL_4dLBqH0dxR5Ih2TxKxCKLjtQvngU7dXY2aXvLppdd4Ecrk14jgEZwFoG0HTLD3wvi97W605_34Mn8kuqBOTBalH1bIHGYm4N2kEkXAKrPqdBuwGaGIcH10pMG428V0hdbrC7vstC5e6CHGD-3CZde0m_-yczW2tj-kA1NQBZxArAURL6R_XQ-9LG9SL3n2dFK3sX-W8tcq8B5qEiqHm; sailthru_pageviews=6; _gat_UA-142576245-4=1; _dc_gtm_UA-142576245-1=1; sailthru_content=6466d62d98c56179f988c58d69b5bdd08226088c167ecf65440e0fc4d786860e4fbb613834ea8cc57feae8bd1e4aaeae4dfcd4abf2c20d3c6d398b4f5a8270b3a6868281678e0269f2e30c72a59520f86ee54343d85ccfb04f65bd847aaafeb5ed016a510cc06d5e79dc1db8608d729a0cf0526f52035a3a6aa562a10b4bae2be1980fd747e58a39e7568037500e1f926a6ebc798cc2c232c5ef8b94521c981527196a8a2587992b557d4b875d9a736c05bccff52c907303182a1f2e536426340e50ea7ed4f2934b0685f4b9504786ce9dc8a9254a0258aae32f96507bf440950df6122f6a027444bc544aeb909fceb6c6c6ef74477fc2a27e5e8ab2a51659a3; sailthru_visitor=108bebf1-2815-489a-8d93-bf931b716ad9; _uetsid=6e456600164d11eda6108b3e96e4d981; _uetvid=9aa4ca703e5f11ec8dfc43f19f496f0e; _clsk=1p9prl6|1659937783011|13|0|i.clarity.ms/collect; _px2=eyJ1IjoiZTZlNGExMDAtMTZkZC0xMWVkLWI5YjUtZjcwNWI5YjI5Y2UwIiwidiI6ImYxZTViZmFhLTBhZTMtMTFlZC05NmQ3LTU3NDk0NDZkNDM2NiIsInQiOjE2NTk5MzgzMjQ1MjUsImgiOiI0YzFkNmNiNzczNmQ3ZTQ1OGIwNGZhMjAxMDRlZGVkODY2NDlkNzNkZDU2OGUxZjIwN2NiNjc2ZGMyODZkOTc2In0=; _px=7gqr6jDyPBKqqd7B6TSIf7843spkV3E9X6ZK0vYEeXjAX/SL99Va6RXI/cuET2P7JJ8B6nW2PnzUOPGw/5xDsQ==:1000:auXAUtiSAkJeK8A0v0S5G4GfcDd9AorUfkrujtTq7VsHwQ7/umLkL1umnA411ylLltV1rKh7R7geqMZw0sDx2ZxPr79y75Koxh+BoI1tmfrtMXWkE4MDKyyKv488LnL+R2JTx8hr2SJJ9/zCr33XbE0Z5efUXNxRXBZzS5/ivrWwygC/8nGlTp346Gqswmrfu+pSNcS/aaUnhgOPTzTl+5/OH4+SI6BeXtZWDCDK9VlbcQkw4TOxG4NFwm21Yd315Ahgnmo0xB3lAx9yzcw6iw==; _pxde=41f57b03e3a1a532664b36ef69c6f86b2585bc66b6d2ae6b79b27696e6a23025:eyJ0aW1lc3RhbXAiOjE2NTk5Mzc4MjgxMDksImZfa2IiOjB9' \
#   -H 'origin: https://seekingalpha.com' \
#   -H 'referer: https://seekingalpha.com/screeners/922a27ae98-To-Download/trading' \
#   -H 'sec-ch-ua: ".Not/A)Brand";v="99", "Google Chrome";v="103", "Chromium";v="103"' \
#   -H 'sec-ch-ua-mobile: ?0' \
#   -H 'sec-ch-ua-platform: "Windows"' \
#   -H 'sec-fetch-dest: empty' \
#   -H 'sec-fetch-mode: cors' \
#   -H 'sec-fetch-site: same-origin' \
#   -H 'user-agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.0.0 Safari/537.36' \
#   --data-raw '{"filter":{},"quant_rank":false,"sort":null,"type":"stock","per_page":1000,"page":1}' \
#   --compressed | jq . > symbols.json


# curl 'https://seekingalpha.com/api/v3/metrics?filter\[fields\]=marketcap_display%2Cdividend_yield%2Cquant_rating%2Cauthors_rating%2Csell_side_rating&filter\[slugs\]=pbr%2Cxom%2Ccve%2Casrt%2Ccvx%2Ccpg%2Csqm%2Cmpc%2Crfp%2Cth%2Cvlo%2Chsbc%2Cusak%2Cdq%2Carlp%2Cnwg%2Casc%2Cdvn%2Cfsk%2Chhs%2Csu%2Cesea%2Cta%2Cjakk%2Ccto%2Cyell%2Cclfd%2Cpagp%2Cobe%2Cchk%2Chdsn%2Cdtegy%2Caavvf%2Cofs%2Cae%2Cmro%2Cplab%2Cvhi%2Cpsx%2Cjob%2Cbxc%2Ctnp%2Cf%2Cpsec%2Csrts%2Cdgii%2Ccgbd%2Ctte%2Cdac%2Cdino%2Cepr%2Clthm%2Cbolt%2Ctmhc%2Ctkomy%2Ccrk%2Cgnk%2Cbldr%2Cacls%2Cgrbk%2Cati%2Cahco%2Cswir%2Ccvlg%2Camd%2Cbelfb%2Cglpg%2Ctrin%2Camr%2Cctra%2Ctsm%2Cgrin%2Cmod%2Cmho%2Cptsi%2Cccrn%2Cmmi%2Cpkoh%2Ceqt%2Cunfi%2Cande%2Cbzh%2Cnabzy%2Ccvi%2Cela%2Clbrt%2Cwcc%2Cvirc%2Cppc%2Ccalm%2Cprph%2Ccls%2Chrb%2Catax%2Casx%2Cvnom%2Ccar%2Cfujhy%2Con%2Cnex%2Cryi%2Cbelfa%2Csup%2Civt%2Cmrk%2Cavgo%2Cadi%2Csanm%2Cpanl%2Chum%2Ctrq%2Cimo%2Ccnq%2Chope%2Ccb%2Cninoy%2Cenlc%2Cevop%2Cepm%2Cclr%2Crgp%2Cebr%2Cchrw%2Camady%2Cnl%2Crex%2Cebr.b%2Chcci%2Crocc%2Csmci%2Cnnn%2Cmerc%2Ctess%2Cnog%2Cwti%2Camrk%2Cusap%2Cpebo%2Ckdny%2Cfang%2Ctalo%2Caetuf%2Cdltr%2Cpeyuf%2Car%2Cvet%2Cmur%2Cvrtv%2Cnecb%2Cdlhc' \
#   -H 'authority: seekingalpha.com' \
#   -H 'accept: */*' \
#   -H 'accept-language: en-US,en;q=0.9' \
#   -H 'cookie: machine_cookie=6071666182064; _igt=eac4b6a3-4f66-497e-db45-26a99710806c; _cls_v=6b22da5b-8cd3-4529-be8d-ea1bf8a5aa9d; _cls_s=364f8035-f84b-4d81-b13e-0563633b4c05:0; __pat=-14400000; _ga=GA1.2.303343256.1658620965; _gcl_au=1.1.770029613.1658620965; _fbp=fb.1.1658620964957.187023299; _cc_id=bbdbd0ec9054d648845a516a9034d5e1; prism_25946650=7ea4c454-399e-4576-b169-279f902e84b8; pxcts=f1e5cbb2-0ae3-11ed-96d7-5749446d4366; _pxvid=f1e5bfaa-0ae3-11ed-96d7-5749446d4366; ga_clientid=303343256.1658620965; g_state={"i_l":0}; user_id=54607428; u_voc=; marketplace_author_slugs=; user_perm=; user_remember_token=d5e1f67e796f682b3e605bf4283e8c33443cdb68; _gac_UA-142576245-3=1.1658695017.Cj0KCQjw2_OWBhDqARIsAAUNTTFT5kgCIX7TbzJHIG1_swVZT2ehZO6tRDKsqB5b6_hnYylaECFxcv0aAgfNEALw_wcB; sailthru_hid=e865f74ef53b891c8276d027693f33ac60e4d2352e08d924735ce6b556c6bc3e5d4403affe742307ff74a234; __pnahc=0; has_paid_subscription=true; ever_pro=1; sapu=12; user_nick=taotree; _hjSessionUser_65666=eyJpZCI6ImM5Nzg3ZGYwLTRlODEtNWUxOS1iY2UwLTcxYjI0N2UyZDA5MCIsImNyZWF0ZWQiOjE2NTkxOTA0OTE2OTMsImV4aXN0aW5nIjp0cnVlfQ==; user_devices=2; session_id=922a0694-00c9-4e0b-ac93-f0b66a5457db; __tae=1659754665990; __tac=; _gid=GA1.2.1902944425.1659875734; panoramaId=ef49906a77f3dcf67f1752487ebc16d53938221439d41fa5de9ff54dc80f7bdb; panoramaId_expiry=1660480534033; _gcl_aw=GCL.1659879239.Cj0KCQjwxb2XBhDBARIsAOjDZ36qEUfFUGzvYR1l_j87V_kwCJhrSVi-d_St16yGuxzQuVhEadbGtZkaAp9cEALw_wcB; _gac_UA-142576245-1=1.1659879239.Cj0KCQjwxb2XBhDBARIsAOjDZ36qEUfFUGzvYR1l_j87V_kwCJhrSVi-d_St16yGuxzQuVhEadbGtZkaAp9cEALw_wcB; _hjCachedUserAttributes=eyJhdHRyaWJ1dGVzIjp7ImxvZ2dlZF9pbiI6dHJ1ZSwibXBfc3ViIjpmYWxzZSwicHJlbWl1bV9zdWIiOnRydWUsInByb19zdWIiOmZhbHNlfSwidXNlcklkIjoiNTQ2MDc0MjgifQ==; _gac_UA-142576245-4=1.1659879240.Cj0KCQjwxb2XBhDBARIsAOjDZ36qEUfFUGzvYR1l_j87V_kwCJhrSVi-d_St16yGuxzQuVhEadbGtZkaAp9cEALw_wcB; _ig=54607428; _clck=g8tqn|1|f3u|0; _hjSession_65666=eyJpZCI6IjFhZmY0YjNiLWIxOTMtNDY5My1hYmFjLTFhZTY4ZDRmZWMzZiIsImNyZWF0ZWQiOjE2NTk5MzczMDcxNDIsImluU2FtcGxlIjpmYWxzZX0=; _hjAbsoluteSessionInProgress=0; user_cookie_key=v9808k; gk_user_access=1*archived*1659937712; gk_user_access_sign=b9a9f74c8724dc98d5a6a0354b9a41c6704a913a; sailthru_pageviews=6; sailthru_content=6466d62d98c56179f988c58d69b5bdd08226088c167ecf65440e0fc4d786860e4fbb613834ea8cc57feae8bd1e4aaeae4dfcd4abf2c20d3c6d398b4f5a8270b3a6868281678e0269f2e30c72a59520f86ee54343d85ccfb04f65bd847aaafeb5ed016a510cc06d5e79dc1db8608d729a0cf0526f52035a3a6aa562a10b4bae2be1980fd747e58a39e7568037500e1f926a6ebc798cc2c232c5ef8b94521c981527196a8a2587992b557d4b875d9a736c05bccff52c907303182a1f2e536426340e50ea7ed4f2934b0685f4b9504786ce9dc8a9254a0258aae32f96507bf440950df6122f6a027444bc544aeb909fceb6c6c6ef74477fc2a27e5e8ab2a51659a3; sailthru_visitor=108bebf1-2815-489a-8d93-bf931b716ad9; _uetsid=6e456600164d11eda6108b3e96e4d981; _uetvid=9aa4ca703e5f11ec8dfc43f19f496f0e; _gat_UA-142576245-4=1; LAST_VISITED_PAGE=%7B%22pathname%22%3A%22https%3A%2F%2Fseekingalpha.com%2Fscreeners%2F922a27ae98-To-Download%22%2C%22pageKey%22%3A%221533b2ee-9361-4932-8314-a69d1fe830ce%22%7D; _pctx=%7Bu%7DN4IgDghg5gpgagSxgdwJIBMQC4QBsBsA1gMYBmAHAMyWkCsEALJYVOSADQgBGATgPbIAzjB4ZseWgE9ClBAwBeDALYAGBIIBu6TJwCuwnoOwA7Xblx6DAZQAuEG-pNmLIQQhswxWU%2BYC%2BQA; __pvi=%7B%22id%22%3A%22v-2022-08-07-22-35-05-455-kIliAeii3xMYHqWw-b130a7b8f8ed2b551d9ed77834de6e3b%22%2C%22domain%22%3A%22.seekingalpha.com%22%2C%22time%22%3A1659938329266%7D; _clsk=1p9prl6|1659938329481|16|0|i.clarity.ms/collect; __tbc=%7Bkpex%7DW6oShuI0nLVBtUkcsYyQNO3HbqO1F1NSDqxDOemOp1I2VzJNwbAM-L7VWV3iXEZeONhV6ZY_iID8vDMKo689yG6JVQyV6-Y5zmElMQcneQqhAoGIqkJOLfI05scsegea; xbc=%7Bkpex%7DDb74YfQ4as8fYTJmh0uxnK8oHL1LXaI6DgepA81zoVYnB9hA2UEAmS9MNnm_cJP-6ljfX0bEp4E7K1DH8uQQIERW73mang6ewYpBhQXOSEDeik40-XmFAwgcqqIREZVZEDEI-chm6EzK1Txp5qsupxjfKfrMCQiu9Vl3pYv0hSDKygnRmIGx9LlNFUKRmxSlYqTSysPgo4wkqA9jBLmpldpKrM-bOoQOf-Z92_xf8fqpitdMam2Mvwjkt9PoNv3x-SAE87qFoFvYfNfTUoEDwt3m9mUEsMExls_3_T9zde17vxzzxBSyzHK5GB8aNCK0MYpw9ZTX1wZxkzUcQQfpuStglPhUe9ChKY_fWdDHKsz3nHEJ1ZYZJRUDFmA3sBXcR6CUZXHFMVUyFKicwFlhA57honc1pUUFrXco79qynJl6Op6uRgBCcPqGMfJ9_KsZoCg4dkKtI7wsAjB3u4OnWxexMWPYmRkPv5MPb08naywqKQdVtABeXIPyxXFnM96E; _px2=eyJ1IjoiMmMyMWUzZDAtMTZkZi0xMWVkLWI5YjUtZjcwNWI5YjI5Y2UwIiwidiI6ImYxZTViZmFhLTBhZTMtMTFlZC05NmQ3LTU3NDk0NDZkNDM2NiIsInQiOjE2NTk5Mzg4MzAzNTAsImgiOiJhM2RjYTk2YWYxMzY1MjdjMWJkOWEyZTUwNTdjODZmMjQ3MjI1OGE2YzNhNTk5ZTY4ODhmZTk0Mzk3YzFhZGJjIn0=; _px=kIyttj33MPBkfdrqpEJQUL2jClIIoOi2RcPiK6sGsYMcbhumee+lJKvN0qrRfcpkHtMw08+U8EK5J+7cmP7gfQ==:1000:kBvUeyW/mOeDo/O3v8f/5dcYHsftTR3Rfbf+tm0tNqyW6ui03njzFWcpQIzgHb0/7aJGtNv2uuu7XkD3t+/JGp/dw1LKFQD+63gf51jQ6oWsBEYIL6/ZfsuoIHgLs3zoH47O2uBa3p17FsQBO/pV/E3IM89050uLMJWbOI3ZNWxL7CdOUEJ4d0tbmtlug66/6Yw/b51l+8bbnR+CKI+gylhsuLdvWDB2uVrz/kZUOkvT5tUwEQ+Bgbw65EQ32/q4Ytparwke3i7oT+34M7LA/w==; _pxde=8de1fbb94e112ab0a300ca6cee3748311e3a7853d63ba7a6992874bb9319ad1d:eyJ0aW1lc3RhbXAiOjE2NTk5MzgzMzAzNTAsImZfa2IiOjB9' \
#   -H 'referer: https://seekingalpha.com/screeners/922a27ae98-To-Download' \
#   -H 'sec-ch-ua: ".Not/A)Brand";v="99", "Google Chrome";v="103", "Chromium";v="103"' \
#   -H 'sec-ch-ua-mobile: ?0' \
#   -H 'sec-ch-ua-platform: "Windows"' \
#   -H 'sec-fetch-dest: empty' \
#   -H 'sec-fetch-mode: cors' \
#   -H 'sec-fetch-site: same-origin' \
#   -H 'user-agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.0.0 Safari/537.36' \
#   --compressed | jq . > metrics.json


# curl 'https://seekingalpha.com/api/v3/ticker_metric_grades?filter\[fields\]=value_category%2Cgrowth_category%2Cprofitability_category%2Cmomentum_category%2Ceps_revisions_category&filter\[slugs\]=pbr%2Cxom%2Ccve%2Casrt%2Ccvx%2Ccpg%2Csqm%2Cmpc%2Crfp%2Cth%2Cvlo%2Chsbc%2Cusak%2Cdq%2Carlp%2Cnwg%2Casc%2Cdvn%2Cfsk%2Chhs%2Csu%2Cesea%2Cta%2Cjakk%2Ccto%2Cyell%2Cclfd%2Cpagp%2Cobe%2Cchk%2Chdsn%2Cdtegy%2Caavvf%2Cofs%2Cae%2Cmro%2Cplab%2Cvhi%2Cpsx%2Cjob%2Cbxc%2Ctnp%2Cf%2Cpsec%2Csrts%2Cdgii%2Ccgbd%2Ctte%2Cdac%2Cdino%2Cepr%2Clthm%2Cbolt%2Ctmhc%2Ctkomy%2Ccrk%2Cgnk%2Cbldr%2Cacls%2Cgrbk%2Cati%2Cahco%2Cswir%2Ccvlg%2Camd%2Cbelfb%2Cglpg%2Ctrin%2Camr%2Cctra%2Ctsm%2Cgrin%2Cmod%2Cmho%2Cptsi%2Cccrn%2Cmmi%2Cpkoh%2Ceqt%2Cunfi%2Cande%2Cbzh%2Cnabzy%2Ccvi%2Cela%2Clbrt%2Cwcc%2Cvirc%2Cppc%2Ccalm%2Cprph%2Ccls%2Chrb%2Catax%2Casx%2Cvnom%2Ccar%2Cfujhy%2Con%2Cnex%2Cryi%2Cbelfa%2Csup%2Civt%2Cmrk%2Cavgo%2Cadi%2Csanm%2Cpanl%2Chum%2Ctrq%2Cimo%2Ccnq%2Chope%2Ccb%2Cninoy%2Cenlc%2Cevop%2Cepm%2Cclr%2Crgp%2Cebr%2Cchrw%2Camady%2Cnl%2Crex%2Cebr.b%2Chcci%2Crocc%2Csmci%2Cnnn%2Cmerc%2Ctess%2Cnog%2Cwti%2Camrk%2Cusap%2Cpebo%2Ckdny%2Cfang%2Ctalo%2Caetuf%2Cdltr%2Cpeyuf%2Car%2Cvet%2Cmur%2Cvrtv%2Cnecb%2Cdlhc&filter\[algos\]\[\]=etf&filter\[algos\]\[\]=dividends&filter\[algos\]\[\]=main_quant&filter\[algos\]\[\]=reit&filter\[algos\]\[\]=reit_dividend' \
#   -H 'authority: seekingalpha.com' \
#   -H 'accept: */*' \
#   -H 'accept-language: en-US,en;q=0.9' \
#   -H 'cookie: machine_cookie=6071666182064; _igt=eac4b6a3-4f66-497e-db45-26a99710806c; _cls_v=6b22da5b-8cd3-4529-be8d-ea1bf8a5aa9d; _cls_s=364f8035-f84b-4d81-b13e-0563633b4c05:0; __pat=-14400000; _ga=GA1.2.303343256.1658620965; _gcl_au=1.1.770029613.1658620965; _fbp=fb.1.1658620964957.187023299; _cc_id=bbdbd0ec9054d648845a516a9034d5e1; prism_25946650=7ea4c454-399e-4576-b169-279f902e84b8; pxcts=f1e5cbb2-0ae3-11ed-96d7-5749446d4366; _pxvid=f1e5bfaa-0ae3-11ed-96d7-5749446d4366; ga_clientid=303343256.1658620965; g_state={"i_l":0}; user_id=54607428; u_voc=; marketplace_author_slugs=; user_perm=; user_remember_token=d5e1f67e796f682b3e605bf4283e8c33443cdb68; _gac_UA-142576245-3=1.1658695017.Cj0KCQjw2_OWBhDqARIsAAUNTTFT5kgCIX7TbzJHIG1_swVZT2ehZO6tRDKsqB5b6_hnYylaECFxcv0aAgfNEALw_wcB; sailthru_hid=e865f74ef53b891c8276d027693f33ac60e4d2352e08d924735ce6b556c6bc3e5d4403affe742307ff74a234; __pnahc=0; has_paid_subscription=true; ever_pro=1; sapu=12; user_nick=taotree; _hjSessionUser_65666=eyJpZCI6ImM5Nzg3ZGYwLTRlODEtNWUxOS1iY2UwLTcxYjI0N2UyZDA5MCIsImNyZWF0ZWQiOjE2NTkxOTA0OTE2OTMsImV4aXN0aW5nIjp0cnVlfQ==; user_devices=2; session_id=922a0694-00c9-4e0b-ac93-f0b66a5457db; __tae=1659754665990; __tac=; _gid=GA1.2.1902944425.1659875734; panoramaId=ef49906a77f3dcf67f1752487ebc16d53938221439d41fa5de9ff54dc80f7bdb; panoramaId_expiry=1660480534033; _gcl_aw=GCL.1659879239.Cj0KCQjwxb2XBhDBARIsAOjDZ36qEUfFUGzvYR1l_j87V_kwCJhrSVi-d_St16yGuxzQuVhEadbGtZkaAp9cEALw_wcB; _gac_UA-142576245-1=1.1659879239.Cj0KCQjwxb2XBhDBARIsAOjDZ36qEUfFUGzvYR1l_j87V_kwCJhrSVi-d_St16yGuxzQuVhEadbGtZkaAp9cEALw_wcB; _hjCachedUserAttributes=eyJhdHRyaWJ1dGVzIjp7ImxvZ2dlZF9pbiI6dHJ1ZSwibXBfc3ViIjpmYWxzZSwicHJlbWl1bV9zdWIiOnRydWUsInByb19zdWIiOmZhbHNlfSwidXNlcklkIjoiNTQ2MDc0MjgifQ==; _gac_UA-142576245-4=1.1659879240.Cj0KCQjwxb2XBhDBARIsAOjDZ36qEUfFUGzvYR1l_j87V_kwCJhrSVi-d_St16yGuxzQuVhEadbGtZkaAp9cEALw_wcB; _ig=54607428; _clck=g8tqn|1|f3u|0; _hjSession_65666=eyJpZCI6IjFhZmY0YjNiLWIxOTMtNDY5My1hYmFjLTFhZTY4ZDRmZWMzZiIsImNyZWF0ZWQiOjE2NTk5MzczMDcxNDIsImluU2FtcGxlIjpmYWxzZX0=; _hjAbsoluteSessionInProgress=0; user_cookie_key=v9808k; gk_user_access=1*archived*1659937712; gk_user_access_sign=b9a9f74c8724dc98d5a6a0354b9a41c6704a913a; sailthru_pageviews=6; sailthru_content=6466d62d98c56179f988c58d69b5bdd08226088c167ecf65440e0fc4d786860e4fbb613834ea8cc57feae8bd1e4aaeae4dfcd4abf2c20d3c6d398b4f5a8270b3a6868281678e0269f2e30c72a59520f86ee54343d85ccfb04f65bd847aaafeb5ed016a510cc06d5e79dc1db8608d729a0cf0526f52035a3a6aa562a10b4bae2be1980fd747e58a39e7568037500e1f926a6ebc798cc2c232c5ef8b94521c981527196a8a2587992b557d4b875d9a736c05bccff52c907303182a1f2e536426340e50ea7ed4f2934b0685f4b9504786ce9dc8a9254a0258aae32f96507bf440950df6122f6a027444bc544aeb909fceb6c6c6ef74477fc2a27e5e8ab2a51659a3; sailthru_visitor=108bebf1-2815-489a-8d93-bf931b716ad9; _uetsid=6e456600164d11eda6108b3e96e4d981; _uetvid=9aa4ca703e5f11ec8dfc43f19f496f0e; _gat_UA-142576245-4=1; LAST_VISITED_PAGE=%7B%22pathname%22%3A%22https%3A%2F%2Fseekingalpha.com%2Fscreeners%2F922a27ae98-To-Download%22%2C%22pageKey%22%3A%221533b2ee-9361-4932-8314-a69d1fe830ce%22%7D; _pctx=%7Bu%7DN4IgDghg5gpgagSxgdwJIBMQC4QBsBsA1gMYBmAHAMyWkCsEALJYVOSADQgBGATgPbIAzjB4ZseWgE9ClBAwBeDALYAGBIIBu6TJwCuwnoOwA7Xblx6DAZQAuEG-pNmLIQQhswxWU%2BYC%2BQA; __pvi=%7B%22id%22%3A%22v-2022-08-07-22-35-05-455-kIliAeii3xMYHqWw-b130a7b8f8ed2b551d9ed77834de6e3b%22%2C%22domain%22%3A%22.seekingalpha.com%22%2C%22time%22%3A1659938329266%7D; _clsk=1p9prl6|1659938329481|16|0|i.clarity.ms/collect; __tbc=%7Bkpex%7DW6oShuI0nLVBtUkcsYyQNO3HbqO1F1NSDqxDOemOp1I2VzJNwbAM-L7VWV3iXEZeONhV6ZY_iID8vDMKo689yG6JVQyV6-Y5zmElMQcneQqhAoGIqkJOLfI05scsegea; xbc=%7Bkpex%7DDb74YfQ4as8fYTJmh0uxnK8oHL1LXaI6DgepA81zoVYnB9hA2UEAmS9MNnm_cJP-6ljfX0bEp4E7K1DH8uQQIERW73mang6ewYpBhQXOSEDeik40-XmFAwgcqqIREZVZEDEI-chm6EzK1Txp5qsupxjfKfrMCQiu9Vl3pYv0hSDKygnRmIGx9LlNFUKRmxSlYqTSysPgo4wkqA9jBLmpldpKrM-bOoQOf-Z92_xf8fqpitdMam2Mvwjkt9PoNv3x-SAE87qFoFvYfNfTUoEDwt3m9mUEsMExls_3_T9zde17vxzzxBSyzHK5GB8aNCK0MYpw9ZTX1wZxkzUcQQfpuStglPhUe9ChKY_fWdDHKsz3nHEJ1ZYZJRUDFmA3sBXcR6CUZXHFMVUyFKicwFlhA57honc1pUUFrXco79qynJl6Op6uRgBCcPqGMfJ9_KsZoCg4dkKtI7wsAjB3u4OnWxexMWPYmRkPv5MPb08naywqKQdVtABeXIPyxXFnM96E; _px2=eyJ1IjoiMmMyMWUzZDAtMTZkZi0xMWVkLWI5YjUtZjcwNWI5YjI5Y2UwIiwidiI6ImYxZTViZmFhLTBhZTMtMTFlZC05NmQ3LTU3NDk0NDZkNDM2NiIsInQiOjE2NTk5Mzg4MzAzNTAsImgiOiJhM2RjYTk2YWYxMzY1MjdjMWJkOWEyZTUwNTdjODZmMjQ3MjI1OGE2YzNhNTk5ZTY4ODhmZTk0Mzk3YzFhZGJjIn0=; _px=kIyttj33MPBkfdrqpEJQUL2jClIIoOi2RcPiK6sGsYMcbhumee+lJKvN0qrRfcpkHtMw08+U8EK5J+7cmP7gfQ==:1000:kBvUeyW/mOeDo/O3v8f/5dcYHsftTR3Rfbf+tm0tNqyW6ui03njzFWcpQIzgHb0/7aJGtNv2uuu7XkD3t+/JGp/dw1LKFQD+63gf51jQ6oWsBEYIL6/ZfsuoIHgLs3zoH47O2uBa3p17FsQBO/pV/E3IM89050uLMJWbOI3ZNWxL7CdOUEJ4d0tbmtlug66/6Yw/b51l+8bbnR+CKI+gylhsuLdvWDB2uVrz/kZUOkvT5tUwEQ+Bgbw65EQ32/q4Ytparwke3i7oT+34M7LA/w==; _pxde=8de1fbb94e112ab0a300ca6cee3748311e3a7853d63ba7a6992874bb9319ad1d:eyJ0aW1lc3RhbXAiOjE2NTk5MzgzMzAzNTAsImZfa2IiOjB9' \
#   -H 'referer: https://seekingalpha.com/screeners/922a27ae98-To-Download' \
#   -H 'sec-ch-ua: ".Not/A)Brand";v="99", "Google Chrome";v="103", "Chromium";v="103"' \
#   -H 'sec-ch-ua-mobile: ?0' \
#   -H 'sec-ch-ua-platform: "Windows"' \
#   -H 'sec-fetch-dest: empty' \
#   -H 'sec-fetch-mode: cors' \
#   -H 'sec-fetch-site: same-origin' \
#   -H 'user-agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.0.0 Safari/537.36' \
#   --compressed | jq . > grades.json

