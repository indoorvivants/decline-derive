package decline_derive

import com.monovore.decline.*
import compiletime.*
import scala.deriving.*
import scala.quoted.*
import cats.kernel.Semigroup

trait Hint[T]

object Hints:
  case object Name extends Hint[String]

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

    ev match
      case '{
            $m: Mirror.SumOf[T] {
              type MirroredElemTypes = elementTypes;
              type MirroredElemLabels = labels
              type MirroredLabel = commandName
            }
          } =>
        // println(m.show)
        val elemInstances = summonInstances[T, elementTypes]
        val elements = Expr.ofList(elemInstances)

        val command = getString[commandName]

        val subcommands = '{
          $elements.map(_.opt).map(Opts.subcommand(_)).reduce(_ orElse _)
        }

        '{
          new CommandApplication[T] {
            override val opt: Command[T] =
              Command($command.toLowerCase(), "???")($subcommands.asInstanceOf)
          }
        }

      // elemInstances.reduce:
      //   (l, r) =>
      //     '{Opts.subcommand}

      // val subcommands =
      //   elemInstances.map: cmd =>
      //     '{}

      // println(summonLabels[labels].map(_.show))

      // val subcommands =
      //   summonLabels

      case '{
            $m: Mirror.ProductOf[T] {
              type MirroredElemTypes = elementTypes;
              type MirroredElemLabels = labels
              type MirroredLabel = commandName
            }
          } =>
        val name = getString[commandName]

        def fieldOpts[T: Type, L: Type]: List[Expr[Opts[?]]] =
          (Type.of[T], Type.of[L]) match
            case ('[elem *: elems], '[elemLabel *: elemLabels]) =>
              val nm = getString[elemLabel]

              Type.of[elem] match
                case '[String] =>
                  '{ Opts.option[String]($nm, "???") } :: fieldOpts[
                    elems,
                    elemLabels
                  ]
                case '[Boolean] =>
                  '{
                    Opts.flag($nm, "???").orFalse
                  } :: fieldOpts[
                    elems,
                    elemLabels
                  ]

            case ('[EmptyTuple], '[EmptyTuple]) => Nil

        val opts = Expr.ofList(fieldOpts[elementTypes, labels])

        val combined = '{
          $opts
            .foldLeft[Opts[Tuple]](Opts(EmptyTuple)): (l, r) =>
              import cats.syntax.all.*
              (l, r).mapN((t, e) => t.:*(e))
            .map($m.fromProduct)
        }

        '{
          new CommandApplication[T] {
            override val opt: Command[T] =
              Command[T]($name.toLowerCase(), "???")($combined)
          }
        }

case class CommandDefinition[T](label: String, opts: Opts[T])
