package decline_derive

import deriving.*, quoted.*, compiletime.*
import com.monovore.decline.Opts
import com.monovore.decline.Command
import com.monovore.decline.Argument
import cats.data.NonEmptyList
import scala.reflect.ClassTag

private[decline_derive] object Macros:
  private def summonInstances[T: Type, Elems: Type](using
      Quotes
  ): List[Expr[CommandApplication[?]]] =
    Type.of[Elems] match
      case '[elem *: elems] =>
        deriveOrSummon[T, elem].asInstanceOf :: summonInstances[T, elems]
      case '[EmptyTuple] => Nil

  private def deriveOrSummon[T: Type, Elem: Type](using
      Quotes
  ): Expr[CommandApplication[Elem]] =
    Type.of[Elem] match
      case '[T] => deriveRec[T, Elem]
      case _    => '{ summonInline[CommandApplication[Elem]] }

  private def deriveRec[T: Type, Elem: Type](using
      Quotes
  ): Expr[CommandApplication[Elem]] =
    Type.of[T] match
      case '[Elem] => '{ error("infinite recursive derivation") }
      case _       => derivedMacro[Elem] // recursive derivation

  private def getString[T: Type](using Quotes): Expr[String] =
    Expr.summon[ValueOf[T]].get match
      case '{ $v } =>
        '{ $v.value.asInstanceOf[String] }

  private def extractDefaultValues[T: Type](using
      Quotes
  ): Map[String, Expr[Any]] =
    import quotes.reflect.*

    val sym = TypeRepr.of[T].typeSymbol
    val comp = sym.companionClass

    if comp == Symbol.noSymbol then return Map.empty

    val mod = Ref(sym.companionModule)
    val typeArgs = TypeRepr.of[T] match
      case AppliedType(_, args) => args
      case _                    => Nil

    // Get parameters with default values
    val paramsWithDefaults = sym.caseFields.zipWithIndex.collect {
      case (field, idx) if field.flags.is(Flags.HasDefault) => (field.name, idx)
    }

    if paramsWithDefaults.isEmpty then return Map.empty

    // Get default value methods from companion
    val body = comp.tree.asInstanceOf[ClassDef].body
    val defaultMethods = body.collect {
      case deff @ DefDef(name, _, _, _)
          if name.startsWith("$lessinit$greater$default") =>
        val methodNum = name
          .stripPrefix("$lessinit$greater$default$")
          .toIntOption
          .getOrElse(-1)
        (methodNum, deff.symbol)
    }.toMap

    // Map parameter names to their default values
    paramsWithDefaults.map { case (paramName, idx) =>
      val methodIdx = idx + 1 // Default methods are 1-indexed
      defaultMethods.get(methodIdx) match
        case Some(symbol) =>
          val ref = mod.select(symbol)
          val applied =
            if typeArgs.nonEmpty then ref.appliedToTypes(typeArgs)
            else ref
          paramName -> applied.asExpr
        case None =>
          paramName -> '{ null }.asExprOf[Any]
      end match
    }.toMap
  end extractDefaultValues

  private final case class Hints(
      name: Option[String] = None,
      short: Option[String] = None,
      help: Option[String] = None,
      flag: Option[Boolean] = None,
      positional: Option[String] = None,
      env: Option[(String, String)] = None,
      debug: Boolean = false,
      defaultValue: Option[Expr[Any]] = None
  )

  private given FromExpr[Name] with
    def unapply(x: Expr[Name])(using Quotes): Option[Name] =
      x match
        case '{ new Name($value) } => Some(Name(value.valueOrAbort))
        case _                     => None
  end given

  private given FromExpr[Flag] with
    def unapply(x: Expr[Flag])(using Quotes): Option[Flag] =
      x match
        case '{ new Flag($value) } => Some(Flag(value.valueOrAbort))
        case _                     => None
  end given

  private given FromExpr[Positional] with
    def unapply(x: Expr[Positional])(using Quotes): Option[Positional] =
      x match
        case '{ new Positional($value) } => Some(Positional(value.valueOrAbort))
        case _                           => None
  end given

  private given FromExpr[Help] with
    def unapply(x: Expr[Help])(using Quotes): Option[Help] =
      x match
        case '{ new Help($value) } => Some(Help(value.valueOrAbort))
        case _                     => None
  end given

  private given FromExpr[Env] with
    def unapply(x: Expr[Env])(using Quotes): Option[Env] =
      x match
        case '{ new Env($name, $help) } =>
          Some(Env(name.valueOrAbort, help.valueOrAbort))
        case _ => None
  end given

  private given FromExpr[Short] with
    def unapply(x: Expr[Short])(using Quotes): Option[Short] =
      x match
        case '{ new Short($value) } => Some(Short(value.valueOrAbort))
        case _                      => None
  end given

  def derivedMacro[T: Type](using Quotes): Expr[CommandApplication[T]] =
    val ev: Expr[Mirror.Of[T]] = Expr.summon[Mirror.Of[T]].get

    import quotes.reflect.*

    val derivedAnnot = TypeRepr.of[DeclineDeriveAnnotation]

    def collectCommandHints(annotations: List[Term]): Hints =
      annotations
        .foldLeft(Hints()): (hints, ann) =>
          if ann.tpe <:< TypeRepr.of[Name] then
            hints.copy(name = Some(ann.asExprOf[Name].valueOrAbort.value))
          else if ann.tpe <:< TypeRepr.of[Help] then
            hints.copy(help = Some(ann.asExprOf[Help].valueOrAbort.value))
          else if ann.tpe <:< TypeRepr.of[Debug] then hints.copy(debug = true)
          else if ann.tpe <:< derivedAnnot then
            report.errorAndAbort(
              s"Commands are not allowed to have `${ann.tpe.show}` annotations - only Name and Help",
              ann.pos
            )
          else hints

    def collectArgAnnotations(terms: List[Term]): Hints =
      terms.foldLeft(Hints()): (hints, ann) =>
        if ann.tpe <:< TypeRepr.of[Name] then
          hints.copy(name = Some(ann.asExprOf[Name].valueOrAbort.value))
        else if ann.tpe <:< TypeRepr.of[Help] then
          hints.copy(help = Some(ann.asExprOf[Help].valueOrAbort.value))
        else if ann.tpe <:< TypeRepr.of[Short] then
          hints.copy(short = Some(ann.asExprOf[Short].valueOrAbort.value))
        else if ann.tpe <:< TypeRepr.of[Flag] then
          hints.copy(flag = Some(ann.asExprOf[Flag].valueOrAbort.default))
        else if ann.tpe <:< TypeRepr.of[Positional] then
          hints.copy(positional =
            Some(ann.asExprOf[Positional].valueOrAbort.metavar)
          )
        else if ann.tpe <:< TypeRepr.of[Env] then
          val annot = ann.asExprOf[Env].valueOrAbort
          hints.copy(env = Some(annot.name -> annot.help))
        else hints

    ev match
      case '{
            $m: Mirror.SumOf[T] {
              type MirroredElemTypes = elementTypes;
              type MirroredElemLabels = labels
              type MirroredLabel = commandName
            }
          } =>
        val cmdHints =
          collectCommandHints(
            TypeRepr
              .of[T]
              .typeSymbol
              .annotations
          )

        val elemInstances = summonInstances[T, elementTypes]
        val elements = Expr.ofList(elemInstances)

        val command = getString[commandName]
        val name = cmdHints.name.fold('{ $command.toLowerCase() })(Expr.apply)
        val help = cmdHints.help.fold(Expr(""))(Expr.apply)

        val derivedSubcommands = '{
          $elements.map(_.command).map(Opts.subcommand(_)).reduce(_ orElse _)
        }

        val cmd = '{
          Command(
            $name,
            $help
          )($derivedSubcommands.asInstanceOf)
        }

        '{
          CommandApplication.Impl(
            $cmd,
            $elements.map(_.command).asInstanceOf
          ): CommandApplication[T]
        }

      case '{
            $m: Mirror.Singleton {
              type MirroredLabel = commandName
              type MirroredType = monoType
            }
          } =>
        val command = getString[commandName]
        val cmdHints = collectCommandHints(
          TypeRepr.of[monoType].termSymbol.annotations
        )

        val name = cmdHints.name.fold('{ $command.toLowerCase() })(Expr.apply)

        val help = cmdHints.help.fold(Expr(""))(Expr.apply)

        val cmd = '{
          Command[T](
            $name,
            $help
          )(Opts.unit.map(_ => $m.fromProduct(EmptyTuple)))
        }

        '{
          CommandApplication.Impl($cmd, Nil)
        }

      case '{
            $m: Mirror.ProductOf[T] {
              type MirroredElemTypes = elementTypes;
              type MirroredElemLabels = labels
              type MirroredLabel = commandName
              type MirroredType = t
            }
          } =>
        val command = getString[commandName]

        val cmdHints =
          collectCommandHints(
            TypeRepr
              .of[T]
              .typeSymbol
              .annotations
          )

        // Extract default values for parameters
        val defaultValues = extractDefaultValues[T]

        val fieldNamesAndAnnotations: List[(String, Hints)] =
          TypeRepr
            .of[T]
            .typeSymbol
            .primaryConstructor
            .paramSymss
            .flatten
            .map: sym =>
              val baseHints = collectArgAnnotations(sym.annotations)
              val hintsWithDefault = defaultValues.get(sym.name) match
                case Some(defaultExpr) =>
                  baseHints.copy(defaultValue = Some(defaultExpr))
                case None => baseHints
              (sym.name, hintsWithDefault)

        val opts =
          Expr.ofList(fieldOpts[elementTypes](fieldNamesAndAnnotations))

        val combined = '{
          $opts
            .foldLeft[Opts[Tuple]](Opts(EmptyTuple)): (l, r) =>
              import cats.syntax.all.*
              (l, r).mapN((t, e) => t.:*(e))
            .map($m.fromProduct)
        }

        val name = cmdHints.name.fold('{ $command.toLowerCase() })(Expr.apply)

        val help = cmdHints.help.fold(Expr(""))(Expr.apply)

        val cmd = '{
          Command[T](
            $name,
            $help
          )($combined)
        }

        '{
          CommandApplication.Impl($cmd, Nil)
        }
    end match
  end derivedMacro

  private def summonArgument[E: Type](fieldName: String)(using Quotes) =
    import quotes.reflect.*
    Expr
      .summon[Argument[E]]
      .getOrElse:
        val tpe = TypeRepr.of[E].show
        report.errorAndAbort(
          s"No instance of `Argument` typeclass was found for type `$tpe`, which is type of field `${fieldName}`"
        )
  end summonArgument

  private def constructOption[E: Type](
      name: String,
      hints: Hints
  )(using Quotes): Expr[Opts[Any]] =
    import quotes.reflect.*

    val isEnum = Implicits.search(TypeRepr.of[Mirror.SumOf[E]]) match
      case _: ImplicitSearchSuccess => true
      case _                        => false

    val isCaseClass = Implicits.search(TypeRepr.of[Mirror.ProductOf[E]]) match
      case _: ImplicitSearchSuccess => true
      case _                        => false


    val hasCommand = Implicits.search(TypeRepr.of[CommandApplication[E]]) match
      case res: ImplicitSearchSuccess =>
        Some(res.tree.asExprOf[CommandApplication[E]])
      case _ => None

    val nm = hints.name match
      case None        => Expr(name)
      case Some(value) => Expr(value)

    val help = hints.help.fold(Expr(""))(Expr.apply)
    val short = hints.short.fold(Expr(""))(Expr.apply)

    Type.of[E] match
      case '[e] if isEnum && hasCommand.isDefined =>
        '{
          Opts.subcommands(
            ${ hasCommand.get }.subcommands.head,
            ${ hasCommand.get }.subcommands.tail*
          )
        }

      case '[e] if isCaseClass && hasCommand.isDefined =>
        '{
          ${hasCommand.get}.command.options
        }
      case '[Boolean] =>
        // Determine the default value for boolean flags
        val defaultBool = hints.defaultValue match
          case Some(defaultExpr) =>
            // If there's an explicit default value, use it
            // Some(defaultExpr.asExprOf[Boolean])
            '{ Option($defaultExpr.asInstanceOf[Boolean]) }
          case None =>
            // Otherwise use the Flag annotation or fallback to orFalse
            '{ None }

        val base = '{
          Opts
            .flag(
              long = $nm,
              help = $help,
              short = $short
            )
        }

        if hints.flag.nonEmpty && hints.defaultValue.nonEmpty then
          report.warning(
            s"Parameter $name has both @Flag(...) and the default value set – the @Flag annotation will be ignored"
          )

        val flg = Expr(hints.flag)

        '{
          val flgDefault = $flg.getOrElse(false)
          val paramDefault = $defaultBool

          paramDefault match
            case Some(true) =>
              $base.orTrue
            case Some(false) =>
              $base.orFalse
            case None =>
              if flgDefault then $base.orTrue
              else $base.orFalse
          end match

        }

      case '[Option[e]] =>
        hints.defaultValue match
          case None =>
            '{
              ${
                constructOption[e](name, hints.copy(defaultValue = None))
              }.orNone
            }
          case Some(defaultExpr) =>
            // If there's a default value for Option[e], wrap the inner type's parser and apply the default
            '{
              ${ constructOption[e](name, hints.copy(defaultValue = None)) }
                .map(Some(_))
                .orElse(Opts($defaultExpr.asInstanceOf[Option[e]]))
            }

      case '[NonEmptyList[e]] =>
        val param = summonArgument[e](name)

        val base = hints.positional match
          case None =>
            '{
              given Argument[e] = $param
              Opts.options[e](
                long = $nm,
                help = $help,
                short = $short
              )
            }
          case Some(value) =>
            val metavar = Expr(value)
            '{
              given Argument[e] = $param
              Opts.arguments[e](metavar = $metavar)
            }

        hints.defaultValue match
          case Some(value) => '{ $base.withDefault($value.asInstanceOf) }
          case None        => base

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

        val base = hints.positional match
          case None =>
            '{
              Opts.option[E](
                $nm,
                $help,
                short = $short
              )(using $param)
            }
          case Some(value) =>
            val metaver = Expr(value)
            '{ Opts.argument[E](metavar = $metaver)(using $param) }
        end base

        val withEnv = hints.env match
          case None =>
            base
          case Some((name, help)) =>
            val envName = Expr(name)
            val envHelp = Expr(help)
            '{
              $base.orElse(
                Opts.env[E](name = $envName, help = $envHelp)(using $param)
              )
            }
        end withEnv

        hints.defaultValue match
          case None =>
            withEnv
          case Some(defaultExpr) =>
            '{
              $withEnv.withDefault($defaultExpr.asInstanceOf[E])
            }
        end match
      case _ =>
        report.errorAndAbort(
          s"Don't know how to handle type ${TypeRepr.of[E].show}"
        )
    end match
  end constructOption

  private def fieldOpts[T: Type](
      annots: List[(String, Hints)]
  )(using Quotes): List[Expr[Opts[?]]] =
    Type.of[T] match
      case ('[elem *: elems]) =>
        val nm = annots.head._1

        constructOption[elem](nm, annots.head._2) ::
          fieldOpts[elems](
            annots.tail
          )

      case other =>
        Nil
  end fieldOpts
end Macros
