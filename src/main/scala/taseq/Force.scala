//package taseq
//
//import MacroCompat.{Context, newTerm, recurse, singleConsArg}
//import scala.language.experimental.macros
//
//import scala.reflect.macros.blackbox
//
//object MacroCompat {
//  type Context = scala.reflect.macros.blackbox.Context
//
//  def newTerm(c: Context, s: String) =
//    c.universe.TermName(c.freshName(s))
//
//  // Somehow this resets the symbols... related to this:
//  // http://stackoverflow.com/questions/20665778/splicing-a-passed-function-body-into-a-macro-rewritten-expression
//  def recurse(c: Context)(on: c.Tree): c.Tree = {
//    import c.universe._
//    val transformer = new Transformer {
//      override def transform(tree: Tree): Tree = tree match {
//        case Ident(name) => Ident(name)
//        case other => super.transform(other)
//      }
//    }
//    transformer.transform(c.untypecheck(on))
//  }
//
//  /*
//   * This is purely to work around a bug with untypecheck creating incorrect trees
//   */
//  def convertUnapplyNonFatalToApply(c: Context)(on: c.Tree): c.Tree = {
//    import c.universe._
//    val transformer = new Transformer {
//      override def transform(tree: Tree): Tree = tree match {
//        case CaseDef(UnApply(Apply(Select(Select(Select(Select(Ident(TermName("scala")), TermName("util")), TermName("control")), TermName("NonFatal")), TermName("unapply")), _), _), _, Apply(Select(New(failureType), c), _))  =>
//          val applyTree = Apply(Select(New(failureType), c), List(Ident(TermName("e"))))
//          // Now use the apply:
//          CaseDef(Apply(Select(Select(Select(Select(Ident(termNames.ROOTPKG), TermName("scala")), TermName("util")), TermName("control")), TermName("NonFatal")), List(Bind(TermName("e"), Ident(termNames.WILDCARD)))), EmptyTree, applyTree)
//        case other => super.transform(other)
//      }
//    }
//    transformer.transform(c.untypecheck(on))
//  }
//  def tryBlock(c: Context)(block: c.Tree): c.Tree = {
//    import c.universe._
//    val failureType = TypeTree(typeOf[scala.util.Failure[Nothing]])
//    val applyTree = Apply(Select(New(failureType), termNames.CONSTRUCTOR), List(Ident(TermName("e"))))
//    // Now use the apply:
//    val matchCase = CaseDef(Apply(Select(Select(Select(Select(Ident(termNames.ROOTPKG), TermName("scala")), TermName("util")), TermName("control")), TermName("NonFatal")), List(Bind(TermName("e"), Ident(termNames.WILDCARD)))), EmptyTree, applyTree)
//
//    Try(block, List(matchCase), EmptyTree)
//  }
//  def singleConsArg(c: Context): c.Tree = {
//    import c.universe._
//    c.prefix.tree match {
//      case Apply(_, List(arg)) => arg
//      case Select(Apply(_, List(arg)), TermName("inline")) => arg
//      case other => c.abort(other.pos, s"Only works immediately following new Class allocation, e.g. (new Foo(x)), but found: ${showRaw(other)}")
//    }
//  }
//
//  /**
//    * Identity map functions come up in for blocks when you yield the last result
//    */
//  def isIdentityFn[T, U](c: Context)(fn: c.Expr[T => U]): Boolean = {
//    import c.universe._
//    fn.tree match {
//      case Function(ValDef(_, argName, _, EmptyTree) :: Nil, Ident(res)) => // literal function definition
//        argName == res
//      case other =>
//        false
//    }
//  }
//
//  def function1Apply[T, U](c: Context)(fn: c.Expr[T => U]): (c.Tree, c.Tree) = {
//    import c.universe._
//    // There are only a few possibilities for expressions of type T => U
//    def go(t: c.Tree): (c.Tree, c.Tree) = t match {
//      case Block(exprs, result) =>
//        val (arg, tree) = go(result)
//        (arg, Block(exprs.map(e => recurse(c)(e)), tree))
//      case Function(ValDef(_, argName, _, EmptyTree) :: Nil, tree) => // literal function definition
//        (Ident(argName), recurse(c)(tree))
//      case other =>
//        c.abort(other.pos, s"requires a literal function, was passed: $other")
//      /*
//    case Ident(fnName) => // passing an existing named function
//      val arg = newTerm(c, "arg")
//      (Ident(arg), Apply(Ident(fnName), List(Ident(arg))))
//    case Select(from, attr) =>
//      // this is a function that is a member
//      val arg = newTerm(c, "arg")
//      (Ident(arg), Apply(Select(from, attr), List(Ident(arg))))
//    case other =>
//      c.abort(other.pos, s"unsupported fn: $other")
//      */
//    }
//    go(fn.tree)
//  }
//
//  def function1Apply[T, U](c: Context)(fn: c.Expr[T => U]): (c.Tree, c.Tree) = {
//    import c.universe._
//    // There are only a few possibilities for expressions of type T => U
//    def go(t: c.Tree): (c.Tree, c.Tree) = t match {
//      case Block(exprs, result) =>
//        val (arg, tree) = go(result)
//        (arg, Block(exprs.map(e => recurse(c)(e)), tree))
//      case Function(ValDef(_, argName, _, EmptyTree) :: Nil, tree) => // literal function definition
//        (Ident(argName), recurse(c)(tree))
//      case other =>
//        c.abort(other.pos, s"requires a literal function, was passed: $other")
//      /*
//    case Ident(fnName) => // passing an existing named function
//      val arg = newTerm(c, "arg")
//      (Ident(arg), Apply(Ident(fnName), List(Ident(arg))))
//    case Select(from, attr) =>
//      // this is a function that is a member
//      val arg = newTerm(c, "arg")
//      (Ident(arg), Apply(Select(from, attr), List(Ident(arg))))
//    case other =>
//      c.abort(other.pos, s"unsupported fn: $other")
//      */
//    }
//    go(fn.tree)
//  }
//  def function2Apply[T, U, V](c: Context)(fn: c.Expr[(T, U) => V]): (c.TermName, c.TermName, c.Tree) = {
//    import c.universe._
//
//    // There are only a few possibilities for expressions of type T => U
//    def go(t: c.Tree): (c.TermName, c.TermName, c.Tree) = t match {
//      case Block(exprs, result) =>
//        val (argT, argU, tree) = go(result)
//        (argT, argU, Block(exprs.map(e => recurse(c)(e)), tree))
//      case Function(ValDef(_, argT, _, EmptyTree) :: ValDef(_, argU, _, EmptyTree) :: Nil, tree) =>
//        // literal function definition
//        (argT, argU, recurse(c)(tree))
//      case other =>
//        c.abort(other.pos, s"requires a literal function, was passed: $other")
//      /*
//      case Ident(fnName) => // passing an existing named function
//        val argT = newTerm(c, "argT")
//        val argU = newTerm(c, "argU")
//        (argT, argU, Apply(Ident(fnName), List(Ident(argT), Ident(argU))))
//      case Select(from, attr) =>
//        // this is a function that is a member
//        val argT = newTerm(c, "argT")
//        val argU = newTerm(c, "argU")
//        (argT, argU, Apply(Select(from, attr), List(Ident(argT), Ident(argU))))
//      case other =>
//        c.abort(other.pos, s"unsupported fn: $other")
//        */
//    }
//    go(fn.tree)
//  }
//}
//
//object Force {
//  def inline[Z](f: () => Z): Z = macro inlineTree[Z]
//
//  private[this] def inlineTree[Z](c: blackbox.Context)(ts: c.Tree, fn: c.Expr[(=> Z)]): c.Expr[Unit] = {
//    import c.universe._
//    //println(showRaw(fn))
//    val (arg, newTree) = MacroCompat.function1Apply(c)(fn)
//    //println(showRaw(newTree))
//    val end = newTerm(c, "end")
//    val pos = newTerm(c, "pos")
//    val step = newTerm(c, "step")
//    val rng = newTerm(c, "range")
//    val tree = q"""{
//      val $rng = $ts
//      var $pos = $rng.start
//      val $end = $rng.end
//      val $step = $rng.step
//      while($pos < $end) {
//        val $arg = $pos
//        $newTree
//        $pos += $step
//      }
//      ()
//    }"""
//    //println(tree)
//    c.Expr[Unit](tree)
//  }
//}
