package decline_derive

enum ArgHint:
  case Name(value: String)
  case Short(value: String)
  case Help(value: String)
  case FlagDefault(value: Boolean)
  case Argument(metavar: String | Option[String] = None)
end ArgHint
