package lms.runtime

import lms.util.Logger

var Log: Logger = Logger.default

case class LMSRuntimeException(msg: String)
    extends RuntimeException(s"[ELMS internal error] $msg")
