package decline_derive

class arg(val hints: (ArgHint.type => ArgHint)*)
    extends annotation.StaticAnnotation:
  private[decline_derive] def getHints = hints.map(_(ArgHint))

class cmd(val hints: (CmdHint.type => CmdHint)*)
    extends annotation.StaticAnnotation:
  private[decline_derive] def getHints = hints.map(_(CmdHint))

