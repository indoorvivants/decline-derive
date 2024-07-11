package decline_derive

import deriving.*, quoted.*, compiletime.*
import com.monovore.decline.Opts
import com.monovore.decline.Command
import com.monovore.decline.Argument
import cats.data.NonEmptyList
import scala.reflect.ClassTag

private[decline_derive] object Macros:

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

  def derivedMacro[T: Type](using Quotes): Expr[CommandApplication[T]] =
    val ev: Expr[Mirror.Of[T]] = Expr.summon[Mirror.Of[T]].get

    import quotes.reflect.*

    val cmdAnnot = TypeRepr.of[cmd]
    val annots = TypeRepr
      .of[T]
      .typeSymbol
      .annotations
      .collectFirst:
        case term if term.tpe =:= cmdAnnot => term.asExprOf[cmd]
      .match
        case None    => '{ Seq.empty[CmdHint] }
        case Some(e) => '{ $e.getHints }

    val hints = CmdHintProvider(annots)

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

        '{
          new CommandApplication[T]:
            override val opt: Command[T] =
              Command(
                ${ hints.name }.getOrElse($command.toLowerCase()),
                ${ hints.help }.getOrElse("")
              )($subcommands.asInstanceOf)
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
              val annotExpr = sym.getAnnotation(argAnnot).get.asExprOf[arg]
              Some(annotExpr)
            else None

        val opts = Expr.ofList(fieldOpts[elementTypes, labels](t))

        val combined = '{
          $opts
            .foldLeft[Opts[Tuple]](Opts(EmptyTuple)): (l, r) =>
              import cats.syntax.all.*
              (l, r).mapN((t, e) => t.:*(e))
            .map($m.fromProduct)
        }

        '{
          new CommandApplication[T]:
            override val opt: Command[T] =
              Command[T](
                ${ hints.name }.getOrElse($name.toLowerCase()),
                ${ hints.help }.getOrElse("")
              )($combined)
        }
    end match
  end derivedMacro

  def summonArgument[E: Type](fieldName: Expr[String])(using Quotes) =
    import quotes.reflect.*
    Expr
      .summon[Argument[E]]
      .getOrElse:
        val tpe = TypeRepr.of[E].show
        report.errorAndAbort(
          s"No instance of `Argument` typeclass was found for type `$tpe`, which is type of field `${fieldName.show}`"
        )
  end summonArgument

  def constructOption[E: Type](
      name: Expr[String],
      hints: ArgHintProvider
  )(using Quotes): Expr[Opts[Any]] =
    import quotes.reflect.*

    Type.of[E] match
      case '[Boolean] =>
        '{
          ${ hints.flag } match
            case None => Opts.flag($name, ${ hints.help }.getOrElse("")).orFalse
            case Some(value) =>
              Opts.flag($name, ${ hints.help }.getOrElse("")).orTrue
        }

      case '[Option[e]] =>
        '{ ${ constructOption[e](name, hints) }.orNone }

      case '[NonEmptyList[e]] =>
        val param = summonArgument[e](name)

        '{
          given Argument[e] = $param

          ${ hints.isArgument } match
            case None =>
              Opts.options[e](
                ${ hints.name }.getOrElse($name),
                ${ hints.help }.getOrElse(""),
                short = ${ hints.short }.getOrElse("")
              )

            case Some(value) =>
              Opts.arguments[e](metavar = value.getOrElse(""))
          end match
        }

      case '[List[e]] =>
        '{
          ${ constructOption[NonEmptyList[e]](name, hints) }
            .map(_.asInstanceOf[NonEmptyList[e]].toList)
        }

      case '[Set[e]] =>
        '{
          ${ constructOption[List[e]](name, hints) }
            .map(_.asInstanceOf[List[e]].toSet)
        }

      case '[Vector[e]] =>
        '{
          ${ constructOption[List[e]](name, hints) }
            .map(_.asInstanceOf[List[e]].toVector)
        }

      case '[Array[e]] =>
        val ct = Expr
          .summon[ClassTag[e]]
          .getOrElse(
            report.errorAndAbort(
              s"No ClassTag available for ${TypeRepr.of[e].show}"
            )
          )

        '{
          given ClassTag[e] = $ct
          ${ constructOption[List[e]](name, hints) }
            .map(_.asInstanceOf[List[e]].toArray)
        }

      case '[e] =>
        val param = summonArgument[E](name)

        '{
          given Argument[E] = $param

          ${ hints.isArgument } match
            case None =>
              Opts.option[E](
                ${ hints.name }.getOrElse($name),
                ${ hints.help }.getOrElse(""),
                short = ${ hints.short }.getOrElse("")
              )

            case Some(value) =>
              Opts.argument[E](metavar = value.getOrElse(""))
          end match

        }
      case _ =>
        report.errorAndAbort(
          s"Don't know how to handle type ${TypeRepr.of[E].show}"
        )
    end match
  end constructOption

  def fieldOpts[T: Type, L: Type](
      annots: List[Option[Expr[arg]]]
  )(using Quotes): List[Expr[Opts[?]]] =
    (Type.of[T], Type.of[L]) match
      case ('[elem *: elems], '[elemLabel *: elemLabels]) =>
        val nm = getString[elemLabel]
        val a = annots.head match
          case None        => '{ Seq.empty[ArgHint] }
          case Some(value) => '{ $value.getHints }

        val hints = ArgHintProvider(a)

        constructOption[elem](nm, hints) ::
          fieldOpts[elems, elemLabels](
            annots.tail
          )

      case other =>
        Nil
  end fieldOpts
end Macros
