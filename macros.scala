package decline_derive

import com.monovore.decline.*
import compiletime.*
import scala.deriving.*
import scala.quoted.*
import cats.kernel.Semigroup

// trait Hints[T]

// object Hints:
//   case object Name extends Hints[String]

enum ArgHint:
  case Name(value: String)
  case Short(value: String)
  case Help(value: String)
  case FlagDefault(value: Boolean)

class arg(val hints: ArgHint*) extends annotation.StaticAnnotation

enum CmdHint:
  case Name(value: String)
  case Help(value: String)

class cmd(val hints: CmdHint*) extends annotation.StaticAnnotation

trait CommandApplication[T]:
  val opt: Command[T]

object CommandApplication:
  inline def derived[T](using Mirror.Of[T]): CommandApplication[T] =
    ${ derivedMacro[T] }

  def summonInstances[T: Type, Elems: Type](using
      Quotes
  ): List[Expr[CommandApplication[?]]] =
    Type.of[Elems] match
      case '[elem *: elems] =>
        deriveOrSummon[T, elem].asInstanceOf :: summonInstances[T, elems]
      case '[EmptyTuple] => Nil

  def summonLabels[Elems: Type](using Quotes): List[Expr[String]] =
    Type.of[Elems] match
      case '[elem *: elems] =>
        val expr = Expr.summon[ValueOf[elem]].get

        '{ $expr.value.asInstanceOf[String] } :: summonLabels[elems]
      // scala.compiletime.constValue[elem]
      case '[EmptyTuple] => Nil

  def deriveOrSummon[T: Type, Elem: Type](using
      Quotes
  ): Expr[CommandApplication[Elem]] =
    Type.of[Elem] match
      case '[T] => deriveRec[T, Elem]
      case _    => '{ summonInline[CommandApplication[Elem]] }

  def deriveRec[T: Type, Elem: Type](using
      Quotes
  ): Expr[CommandApplication[Elem]] =
    Type.of[T] match
      case '[Elem] => '{ error("infinite recursive derivation") }
      case _       => derivedMacro[Elem] // recursive derivation

  def getString[T: Type](using Quotes): Expr[String] =
    Expr.summon[ValueOf[T]].get match
      case '{ $v } =>
        '{ $v.value.asInstanceOf[String] }

  private def derivedMacro[T: Type](using Quotes): Expr[CommandApplication[T]] =
    val ev: Expr[Mirror.Of[T]] = Expr.summon[Mirror.Of[T]].get

    import quotes.reflect.*

    val cmdAnnot = TypeRepr.of[cmd]
    val annots = TypeRepr
      .of[T]
      .typeSymbol
      .annotations
      .collectFirst:
        case term if term.tpe =:= cmdAnnot => term.asExprOf[cmd]

    inline def getHint[T: Type](
        inline f: PartialFunction[CmdHint, T]
    ): Expr[Option[T]] =
      annots match
        case None => Expr(None)
        case Some(e) =>
          '{ $e.hints.collectFirst(f) }
    end getHint

    ev match
      case '{
            $m: Mirror.SumOf[T] {
              type MirroredElemTypes = elementTypes;
              type MirroredElemLabels = labels
              type MirroredLabel = commandName
            }
          } =>
        val elemInstances = summonInstances[T, elementTypes]
        val elements = Expr.ofList(elemInstances)

        val command = getString[commandName]

        val subcommands = '{
          $elements.map(_.opt).map(Opts.subcommand(_)).reduce(_ orElse _)
        }

        val nameOverride =
          getHint:
            case CmdHint.Name(value) => value

        val helpOverride =
          getHint:
            case CmdHint.Help(value) => value

        '{
          new CommandApplication[T] {
            override val opt: Command[T] =
              Command(
                $nameOverride.getOrElse($command.toLowerCase()),
                $helpOverride.getOrElse("")
              )($subcommands.asInstanceOf)
          }
        }

      case '{
            $m: Mirror.ProductOf[T] {
              type MirroredElemTypes = elementTypes;
              type MirroredElemLabels = labels
              type MirroredLabel = commandName
            }
          } =>
        val name = getString[commandName]

        val argAnnot = TypeRepr.of[arg].typeSymbol

        val t = TypeRepr
          .of[T]
          .typeSymbol
          .primaryConstructor
          .paramSymss
          .flatten
          .map: sym =>
            if sym.hasAnnotation(argAnnot) then
              val fieldNameExpr = Expr(sym.name.asInstanceOf[String])
              val annotExpr = sym.getAnnotation(argAnnot).get.asExprOf[arg]
              Some(annotExpr)
            else None

        def fieldOpts[T: Type, L: Type](
            annots: List[Option[Expr[arg]]]
        ): List[Expr[Opts[?]]] =
          (Type.of[T], Type.of[L]) match
            case ('[elem *: elems], '[elemLabel *: elemLabels]) =>
              val nm = getString[elemLabel]
              val a = annots.head

              inline def getHint[T: Type](
                  inline f: PartialFunction[ArgHint, T]
              ): Expr[Option[T]] =
                a match
                  case None => Expr(None)
                  case Some(e) =>
                    '{ $e.hints.collectFirst(f) }
              end getHint

              val nameOverride = getHint:
                case ArgHint.Name(v) => v

              val shortOverride = getHint:
                case ArgHint.Short(v) => v

              val help =
                val configured = getHint:
                  case ArgHint.Help(v) => v

                '{ $configured.getOrElse("") }

              val name = '{ $nameOverride.getOrElse($nm) }

              def constructArg[E: Type] =
                val argument = Expr.summon[Argument[E]]

                argument match
                  case None =>
                    val tpe = TypeRepr.of[E].show
                    report.errorAndAbort(
                      s"No instance of `Argument` typeclass was found for type `$tpe`, which is type of field `${nm.valueOrAbort}`"
                    )
                  case Some(value) =>
                    '{
                      given Argument[E] = $value
                      Opts.option[E](
                        $name,
                        $help,
                        short = $shortOverride.getOrElse("")
                      )
                    }
              end constructArg

              Type.of[elem] match
                case '[Option[e]] =>
                  val raw = constructArg[e]
                  '{ $raw.orNone } :: fieldOpts[elem, elemLabels](annots.tail)

                case '[Boolean] =>
                  val flagOverride =
                    getHint:
                      case ArgHint.FlagDefault(value) => value

                  '{
                    $flagOverride match
                      case None        => Opts.flag($name, $help).orFalse
                      case Some(value) => Opts.flag($name, $help).orTrue
                  } :: fieldOpts[
                    elems,
                    elemLabels
                  ](annots.tail)

                case '[e] =>
                  constructArg[e] :: fieldOpts[
                    elems,
                    elemLabels
                  ](annots.tail)

            case other =>
              Nil

        val opts = Expr.ofList(fieldOpts[elementTypes, labels](t))

        val combined = '{
          $opts
            .foldLeft[Opts[Tuple]](Opts(EmptyTuple)): (l, r) =>
              import cats.syntax.all.*
              (l, r).mapN((t, e) => t.:*(e))
            .map($m.fromProduct)
        }

        val nameOverride =
          getHint:
            case CmdHint.Name(value) => value

        val helpOverride =
          getHint:
            case CmdHint.Help(value) => value

        '{
          new CommandApplication[T] {
            override val opt: Command[T] =
              Command[T](
                $nameOverride.getOrElse($name.toLowerCase()),
                $helpOverride.getOrElse("")
              )($combined)
          }
        }

case class CommandDefinition[T](label: String, opts: Opts[T])
