# 99_session_info.R
source(here::here("scripts","00_setup.R"))
writeLines(capture.output(sessionInfo()), con = here::here("results","sessionInfo.txt"))
