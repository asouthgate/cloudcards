library("log4r")

logger <- logger(appenders = console_appender(json_log_layout()))
