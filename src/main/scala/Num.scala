//noinspection SpellCheckingInspection
trait Num{
  override def toString: String = this match{
    case Const(n) => n.toString
    case Add(n1, n2) => s"$n1 + $n2"
    case Sub(n1, n2) => s"$n1 - $n2"
    case Mult(n1, n2) => s"$n1 * $n2"
    case Div(n1, n2) => s"$n1 / $n2"
    case Expr(s) => s.mkString
    case Neg(n) => s"~$n"
  }
  def eval: Maybe[Double] = this match {
    case Const(n) => Result(n)
    case Add(n1, n2) =>
      val n1v: Maybe[Double] = n1.eval
      val n2v: Maybe[Double] = n2.eval
      n1v.comb(n2v, _ + _)

    case Mult(n1, n2) =>
      val n1v: Maybe[Double] = n1.eval
      val n2v: Maybe[Double] = n2.eval
      n1v.comb(n2v, _ * _)

    case Sub(n1, n2) =>
      val n1v: Maybe[Double] = n1.eval
      val n2v: Maybe[Double] = n2.eval
      n1v.comb(n2v, _ - _)

    case Div(n1, n2) =>
      val n1v: Maybe[Double] = n1.eval
      val n2v: Maybe[Double] = n2.eval
      n2v match {
        case Result(0) => Raise("Division by Zero")
        case _ => n1v.comb(n2v, _ / _)
      }

    case Neg(n) =>
      n.eval.flatMap(v => Result(-v))

    case Expr(sp) =>
      val decAdmissibles: List[Char] = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.')
      val decStartAdmissibles: List[Char] = '~' :: decAdmissibles
      //It is okay for a decimal to start with ~
      def strIsDecimal(str: List[Char]): Boolean = {
        str.nonEmpty &&
        (
          str.head == '~' && (str.tail.nonEmpty && str.tail.forall(decAdmissibles.contains(_))) ||
          str.head != '~' && str.forall(decAdmissibles.contains(_))
        )
      }
      def strIsParen(str: List[Char]): Boolean = {
        str.last == ')' &&
        str.head == '(' || (str.tail.nonEmpty && str.head == '~' && str(1)=='(')
      }
      def strStartsWithDecimal(str: List[Char]): Boolean = {
        val posDec = str.takeWhile(decStartAdmissibles.contains(_))
        posDec.nonEmpty && strIsDecimal(posDec)
      }
      def strStartsWithParen(str: List[Char]): Boolean = {
        str.nonEmpty &&
        (str.head == '(' || (str.head == '~' && str.tail.nonEmpty && str.tail.head == '('))
      }
      val ops: List[Char] = List('+', '-', '*', '/', '~') //Negation is denoted by ~
      val binOps: List[Char] = ops.filter(_ != '~')
      def InvalidStartError[A](str: List[Char], orig: String): Maybe[A] = {
        Raise(
          "Expression begins with invalid character\n" +
            s"Expression ${str.mkString} cannot begin with ${str.head}\n" +
            s"Called from: $orig."
        )
      }

      val noConsecNumsOrOps: Boolean = {
        val singSpaceStr = sp.mkString.trim.replaceAll(" +", " ")
        val cutBySpace = singSpaceStr.split(" ")

        def check_acc(past_val: String, cur_list: List[String]): Boolean = {
          val opsAsStrs = ops.map(c => s"$c")
          val binOpsAsStrs = binOps.map(c => s"$c")
          cur_list match {
            case Nil => true
            case cur_val :: rest => if (
              opsAsStrs.contains(past_val) && binOpsAsStrs.contains(cur_val) ||
              strIsDecimal(past_val.toList) && strIsDecimal(cur_val.toList)
            ) false else check_acc(cur_val, rest)
          }
        }

        check_acc(cutBySpace.head, cutBySpace.toList.drop(1))
      }
      if (!noConsecNumsOrOps) Raise("Consecutive numbers or operators in expression.")
      else {
        val s = sp.filter(_ != ' ') //Remove all spaces

        def cutNumberOut(string: List[Char], i: Int): Maybe[(List[Char], List[Char])] = {

          if (!decAdmissibles.contains(string(i)) && string(i) != '~') Raise(
            "Double does not begin at given index. \n" +
              s"${string(i)} at position $i in $string is not valid."
          )
          val newStr = string.drop(i)
          val numStr = newStr.takeWhile(decStartAdmissibles.contains(_))
          val numAfterNegs = numStr.dropWhile(_ == '~')
          if(numAfterNegs.count(_ == '~') > 0) Raise(
            "Num cannot have negations signs within it.\n"+
            s"${string.mkString} contains ${numStr.mkString} which contains negation signs within it."
          )
          else Result((numStr, string.take(i) ++ string.drop(i+numStr.length)))
        }

        /**
          *
          * @param string: List[Char]- String to find matching parentheses for.
          * @param i: Int- Index of parentheses to find match for.
          * @return Maybe[Double]: Either a Result object containing the index of the matching paren
          *         or Raise("No Matching Parentheses") if no matching paren is found.
          */
        def findMatchingParen(string: List[Char], i: Int): Maybe[Int] = {
          //println(s"string: $string, i: $i")
          def f_acc(ci: Int, cpns: Int, cstr: List[Char]): Maybe[Int] = {
            //println(s"cstr: $cstr")
            if (cpns == 0) Result(ci - 1)
            else {
              cstr match {
                case Nil => Raise("No Matching Parentheses")
                case '(' :: rest => f_acc(ci + 1, cpns + 1, rest)
                case ')' :: rest => f_acc(ci + 1, cpns - 1, rest)
                case _ :: rest => f_acc(ci + 1, cpns, rest)
              }
            }
          }
          if(!strStartsWithParen(string.drop(i))) Raise(
            "String does not start with paren at given index.\n" +
              s"There is no paren at pos: $i for ${string.mkString}"
          )
          else{
            val hasNeg: Boolean = string.drop(i).head == '~'
            if(hasNeg) {
              f_acc(i+2, 1, string.drop(i+2))
            }
            else f_acc(i+1, 1, string.drop(i+1))
          }
        }

        /**
          * Cuts parentheses out of string and returns what is inside parentheses and the string without parentheses.
          * @param string: List[Char]- The string to parse parentheses out of.
          * @param i: Int- The index where parentheses begin.
          * @return Maybe[(List[Char], List[Char])
          *         Returns both the string inside of the parentheses and the string without the parentheses in a pair
          *         wrapped in Result if the string has a matching parentheses, or a Raise if any errors occur.
          */
        def cutInsideParens(string: List[Char], i: Int): Maybe[(List[Char], List[Char])] = {
          val mtchIndex: Maybe[Int] = findMatchingParen(string, i)
          val shift: Int = if(string(i) == '(') 1 else 2
          mtchIndex.flatMap(mi => {
            Result((string.slice(i + shift, mi), string.take(i) ++ string.drop(mi+1)))
          })
        }

        /**
          * Parse an operator character to the operator it represents.
          * @param op: Char- The character of the operator
          * @return (Num, Num) => Num
          *         The operator the character represents.
          * @todo   Make this function total by encapsulating it in a Maybe
          */
        def parseOpToObj(op: Char): (Num, Num) => Num = {
          op match {
            case '+' => Add
            case '-' => Sub
            case '*' => Mult
            case '/' => Div
          }
        }

        def replaceInsideParens(string: List[Char]): List[Char] = {
          def replace_acc(cur_string: List[Char], rem_string: List[Char], num_parens: Int): List[Char] = {
            rem_string match {
              case Nil => cur_string
              case '(' :: rest =>
                if (num_parens == 0)
                  replace_acc(cur_string ++ List('('), rest, num_parens + 1)
                else
                  replace_acc(cur_string ++ List('_'), rest, num_parens + 1)

              case ')' :: rest =>
                if(num_parens == 1)
                  replace_acc(cur_string ++ List(')'), rest, num_parens - 1)
                else
                  replace_acc(cur_string ++ List('_'), rest, num_parens - 1)

              case char :: rest =>
                if(num_parens == 0)
                  replace_acc(cur_string ++ List(char), rest, num_parens)
                else
                  replace_acc(cur_string ++ List('_'), rest, num_parens)

              }
            }
          replace_acc(Nil, string, 0)
        }
        /**
          * Return the number of operators in a given expression.
          * @param string: List[Char]- The expression to find operators in.
          * @return Int
          *         The total number of operators (+ - * /) in the expression.
          */
        def numOpsInString(string: List[Char]): Int = {
          replaceInsideParens(string).count(binOps.contains(_))
        }
        def parseConstToNum(string: List[Char]): Maybe[Num] = {
          if(!strIsDecimal(string)) Raise(
            s"Called parseConstToNum on ${string.mkString} which is not a const."
          )
          else if(string.count(_ == '.')>1) Raise(
            "Decimal cannot have more than two decimal points.\n"+
              s"${string.mkString} has more than one point."
          )
          else{
            cutNumberOut(string, 0).flatMap({
              case (numStr, _) =>
                val condNumStr = numStr.mkString.replace("~~", "").toList
                if(condNumStr.head == '~') Result(Neg(Const(condNumStr.drop(1).mkString.toDouble)))
                else Result(Const(condNumStr.mkString.toDouble))
            })
          }
        }
        def parseParenToNum(string: List[Char]): Maybe[Num] = {
          //println(s"Parsing paren ${string.mkString} to num")
          if(!strIsParen(string)) Raise(
            s"Called parseParenToNum on ${string.mkString} which is not a paren."
          )
          else if(string.head == '~') parseParenToNum(string.drop(1)).flatMap(n => Result(Neg(n)))
          else parseToNum(string.slice(1, string.length - 1))
        }
        /**
          * Parses an expression to the Num it represents.
          * @param string: List[Char]- The expression to parse.
          * @return Maybe[Num]
          *         The Num represented by the expression if it is valid, else a Raise describing what
          *         invalidates the expression.
          */
        def parseToNum(string: List[Char]): Maybe[Num] = {
          val stringSingleNeg: List[Char] = string.mkString.replace("~~", "").toList
          //println(s"String to parse: $stringSingleNeg")
          if(stringSingleNeg.isEmpty) Raise("Cannot parse empty string.")
          else if(strIsParen(stringSingleNeg)) parseParenToNum(stringSingleNeg)
          else if (strIsDecimal(stringSingleNeg)) parseConstToNum(stringSingleNeg)
          else{
            def simplify_expr(startNum: Maybe[Num], expr: List[Char]): Maybe[Num] = {
              def simp_until_2(cur_num: Maybe[Num], cur_expr: List[Char]): Maybe[Num] = {
                //println(s"startNum: $startNum, expr: $expr, curNum: $cur_num, curExpr: $cur_expr")
                if(cur_expr.isEmpty) cur_num
                else if(numOpsInString(cur_expr) == 1) {
                  val op = cur_expr.head
                  val opObj = parseOpToObj(op)
                  val endStr: List[Char] = cur_expr.drop(1)
                  if(endStr.isEmpty) {
                    startNum.flatMap(startNumN =>
                      Raise(
                        "Operator does not have 2 operands." +
                          s"\nA $op has less than two operands in $startNumN${expr.mkString}"
                      )
                    )
                  }
                  else{
                    val endNum: Maybe[Num] = {
                      if(strIsDecimal(endStr)){
                        parseConstToNum(endStr)
                      }
                      else if(strIsParen(endStr)){
                        cutInsideParens(endStr, 0).flatMap({
                          case (parenStr, _) =>
                            parseToNum(parenStr).flatMap(parenNum => {
                              if(endStr.head == '~') Result(Neg(parenNum))
                              else Result(parenNum)
                            })
                        })
                      }
                      else{
                        InvalidStartError(endStr, "simp_until_2 1 op")
                      }
                    }
                    cur_num.comb(endNum, opObj(_, _))
                  }
                }
                else{
                  val md = List('*', '/')
                  val pm = List('+', '-')
                  val op1: Char = cur_expr.head
                  val strWoFirstOp = cur_expr.drop(1)
                  val firstNumAndStrWOM: Maybe[(Num, List[Char])] = {
                    if(strStartsWithDecimal(strWoFirstOp)){
                      cutNumberOut(strWoFirstOp, 0).flatMap({
                        case (numStr, strWoNum) =>
                          parseConstToNum(numStr).flatMap(numNum => Result((numNum, strWoNum)))
                      })
                    }
                    else if(strStartsWithParen(strWoFirstOp)){
                      val insideParensM = cutInsideParens(strWoFirstOp, 0)
                      insideParensM.flatMap(
                        { case (parenStr, strwoparen) =>
                          parseParenToNum(parenStr).flatMap(n => Result((n, strwoparen)))
                        }
                      )
                    }
                    else{
                      InvalidStartError(cur_expr, "simp_until_2")
                    }
                  }
                  val firstNumM: Maybe[Num] = firstNumAndStrWOM.flatMap({case (n, _) => Result(n)})
                  firstNumAndStrWOM.flatMap({case (_, strWoFNum) =>
                    //println(s"firstNum: $firstNumM, new_exp: $strWoFNum")
                    val op2: Char = strWoFNum.head
                    val combOp = parseOpToObj(op1)


                    if(md.contains(op1) || (pm.contains(op1) && pm.contains(op2))){
                      //println(s"op1: $op1, op2: $op2")
                      simp_until_2(cur_num.comb(firstNumM, combOp(_, _)), strWoFNum)
                    }
                    else {
                      //println("in else")
                      //We are guaranteed for first op to be + or - and op2 to be * or /
                      def allConsecMults(expr: List[Char]): Maybe[List[Char]] = {
                        def consec_mul_acc(
                                            expr_acc: List[Char],
                                            cur_expr: List[Char]
                                          ): Maybe[List[Char]] = {
                          //println(s"acc: $expr_acc, cur: $cur_expr")
                          if(cur_expr.isEmpty) Result(expr_acc)
                          else if(pm.contains(cur_expr.head)) Result(expr_acc)
                          else {
                            val op: Char = cur_expr.head
                            val str_wo_md: List[Char] = cur_expr.drop(1)
                            //println(s"op: $op, strWoMd: $str_wo_md")
                            if(strStartsWithDecimal(str_wo_md)) {
                              val strNumM: Maybe[(List[Char], List[Char])] = cutNumberOut(str_wo_md, 0)
                              strNumM.flatMap(
                                {case (numStr, strWoNum) =>
                                  consec_mul_acc(expr_acc ++ (op :: numStr), strWoNum)
                                }
                              )
                            }
                            else if(strStartsWithParen(str_wo_md)){
                              val strParenM: Maybe[(List[Char], List[Char])] = cutInsideParens(str_wo_md, 0)
                              strParenM.flatMap(
                                {case (parenStrWoParens, strWoParen) =>
                                  val parenStr = '(' :: (parenStrWoParens ++ List(')'))
                                  val pSrtWNeg = if(str_wo_md.head == '~') '~' :: parenStr else parenStr
                                  consec_mul_acc(expr_acc ++ (op :: pSrtWNeg), strWoParen)
                                }
                              )
                            }
                            else{
                              InvalidStartError(cur_expr, "consec_mul_acc")
                            }
                          }
                        }
                        consec_mul_acc(List(), expr)
                      }
                      firstNumAndStrWOM.flatMap({case (_, strWoNum) =>
                        val multSM: Maybe[List[Char]] = allConsecMults(strWoNum)
                        //println(s"consecMuls: $multSM")
                        multSM.flatMap(multStr => {
                          val secondValM = simplify_expr(firstNumM, multStr)
                          val newCur = cur_num.comb(secondValM, combOp(_, _))
                          val newExpr = strWoNum.drop(multStr.length)
                          //println(s"secondValM: $secondValM, oldCur: $cur_num, newCur: $newCur, newExpr: $newExpr")
                          simp_until_2(newCur, newExpr)
                        })
                      })
                    }
                  })
                }
              }
              simp_until_2(startNum, expr)
            }
            if(strStartsWithDecimal(stringSingleNeg)){
              val numM: Maybe[(List[Char], List[Char])] = cutNumberOut(stringSingleNeg, 0)
              numM.flatMap(
                {case (numStr, strWoNum) =>
                  val startN: Maybe[Num] = parseConstToNum(numStr)
                  simplify_expr(startN, strWoNum)
                }
              )
            }
            else if(strStartsWithParen(stringSingleNeg)){
              val isNeg: Boolean = stringSingleNeg.head == '~'
              val parenPartM: Maybe[(List[Char], List[Char])] = {
                if(!isNeg) cutInsideParens(stringSingleNeg, 0)
                else cutInsideParens(stringSingleNeg, 0)
              }
              parenPartM.flatMap({
                case (parenPart, strWoParen) =>
                  val parenVal: Maybe[Num] = parseParenToNum(parenPart)
                  val startN: Maybe[Num] = if(isNeg)
                    parenVal.flatMap(parenNum => Result(Neg(parenNum)))
                  else parenVal
                  simplify_expr(startN, strWoParen)
                }
              )
            }
            else{
              InvalidStartError(stringSingleNeg, "simp_expr")
            }
          }
        }
        parseToNum(s).flatMap(_.eval)
      }
    }
}

case class Const(f: Double) extends Num
case class Add(n1: Num, n2: Num) extends Num
case class Sub(n1: Num, n2: Num) extends Num
case class Mult(n1: Num, n2: Num) extends Num
case class Div(n1: Num, n2: Num) extends Num
case class Expr(exp: List[Char]) extends Num
case class Neg(n: Num) extends Num


