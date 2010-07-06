package uk.ac.ebi.sr
package interpreter

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import model.RVal.{RDouble, RInt}

/**
 *
 * Date: 21.05.2010
 * @author Taalai Djumabaev
 */

@RunWith(classOf[JUnitRunner])
class ParserSuite extends FunSuite {

  test("expression parsing test 1") {
    val input =
        """ function() max();
            5 + 3.4          ; """
    val res = RParser.parseUnwrap(input, RParser.rProgram) match {
      case Block(
        List(
          FunDecl(
            List(),
            FunCall(
              Var("max"),
              List(NoneArg))),
          Add(
            Num(_),
            Num(_)),
          _*
        )) => true
      case _ => false
      }
    assert(res)
  }

  test("expression parsing test 2") {
    val input =
        """  { beeee(); };
                           # this is a comment

             plot(x1,type = "b",col="red",pch=5); """
    val res = RParser.parseUnwrap(input, RParser.rProgram) match {
      case Block(
        List(
          Block(List(
            FunCall(
              Var("beeee"),
              List(NoneArg)))),
          FunCall(
            Var("plot"),
            List(
              CallArg(Var("x1")),
              CallArgDef("type",Lit("b")),
              CallArgDef("col",Lit("red")),
              CallArgDef("pch",Num(_))
            )),
          _*           // can be eof. depends on the platform
        )) => true
      case _ => false
    }
    assert(res)
  }

  test("expression parsing test 3") {
    val input =
        """plot(
              x2,
              type = "b",
              col="green",
              pch="$"); #;;;;;
           function(m) max(abs(m));   """
    val res = RParser.parseUnwrap(input, RParser.rProgram) match {
      case Block(
        List(
          FunCall(
            Var("plot"),
            List(
              CallArg(Var("x2")),
              CallArgDef("type",Lit("b")),
              CallArgDef("col",Lit("green")),
              CallArgDef("pch",Lit("$"))
            )),
          FunDecl(
            List(DeclArg("m")),
            FunCall(
              Var("max"),
              List(
                CallArg(FunCall(
                    Var("abs"),
                    List(CallArg(Var("m")))))
              ))),
          _*      //can be eof. depends on the platform
        )) => true
      case _ => false
    }
    assert(res)
  }

  test("expression parsing test 4") {
    val input =
        """ 5 + 3.4            ;
            seq(0.01,0.2, 0.03) ;

            function(m) {
              eigen(m,only.values = TRUE);#;;;;;;; ggh
              max(abs(values));
            }; """
    val res = RParser.parseUnwrap(input, RParser.rProgram) match {
      case Block(
        List(
          Add(
            Num(_),
            Num(_)),
          FunCall(
            Var("seq"),
            List(
              CallArg(Num(_)),
              CallArg(Num(_)),
              CallArg(Num(_))
            )),
          FunDecl(
            List(DeclArg("m")),
            Block(List(
              FunCall(
                Var("eigen"),
                List(
                  CallArg(Var("m")),
                  CallArgDef("only.values",True)
                )),
              FunCall(
                Var("max"),
                List(
                  CallArg(FunCall(
                    Var("abs"),List(CallArg(Var("values")))))))
            ))),
          _*
        )) => true
      case _ => false
    }
    assert(res)
  }

  test("expression parsing test 5") {
    val input =
        """ train <- data.frame(l[c(1:(n/4),(3*n/4+1):n),]);
            obj <- tune(svm, Class, data = train,
            ranges = list(gamma = seq(0.01,0.2, 0.03), nu = seq(0.01,0.2,0.03)),
	          type="nu-classification", tunecontrol=tune.control(cross=2));"""
    val res = RParser.parseUnwrap(input, RParser.rProgram) match {
      case Block(
        List(
          AssignToLeft(
            Var("train"),
            FunCall(
              Var("data.frame"),
              List(
                CallArg(Index(
                  Var("l"),
                  List(IndexArg(
                    FunCall(
                      Var("c"),
                      List(
                        CallArg(Sequence(
                          Num(_),
                          Div(Var("n"),Num(_)))),
                        CallArg(Sequence(
                          Add(
                            Div(
                              Mul(Num(_),Var("n")),
                              Num(_)),
                            Num(_)),
                          Var("n")))
                      ))),
                    EmptyIndex))))
              )),
          AssignToLeft(
            Var("obj"),
            FunCall(
              Var("tune"),
              List(
                CallArg(Var("svm")),
                CallArg(Var("Class")),
                CallArgDef("data",Var("train")),
                CallArgDef(
                  "ranges",
                  FunCall(
                    Var("list"),
                    List(CallArgDef(
                      "gamma",
                      FunCall(
                        Var("seq"),
                        List(
                          CallArg(Num(_)),
                          CallArg(Num(_)),
                          CallArg(Num(_))))),
                    CallArgDef(
                      "nu",
                      FunCall(
                        Var("seq"),
                        List(
                          CallArg(Num(_)),
                          CallArg(Num(_)),
                          CallArg(Num(_)))))
                  ))),
                CallArgDef("type",Lit("nu-classification")),
                CallArgDef(
                  "tunecontrol",
                  FunCall(
                    Var("tune.control"),
                    List(CallArgDef("cross",Num(_)))))
              ))),
          _*
        )) => true
      case _ => false
    }
    assert(res)
  }

  test("function declaration bug fix ") {
    val input = "wer = function(x) {x*2} + 3"
    val res = RParser.parseUnwrap(input, RParser.rProgram) match {
      case Block(
        List(
          Assign(
            Var("wer"),
            FunDecl(
              List(DeclArg("x")),
              Add(
                Block(List(
                  Mul(Var("x"),Num(_)))),
                Num(_)
              )
          )),
          _*
        )) => true
      case _ => false
    }
    assert(res)
  }

  test("new line delimiter accepting test") {
    val input =
        """ plot(x2,type =
            "b",
            col
            =
            "green",pch="$")

            function(m)
            max(
            abs(
            m
            )
            )

            function(

            ) max()

            5 + 3.4
        """
    val res = RParser.parseUnwrap(input, RParser.rProgram) match {
      case Block(
        List(
          FunCall(
            Var("plot"),
            List(
              CallArg(Var("x2")),
              CallArgDef("type",Lit("b")),
              CallArgDef("col",Lit("green")),
              CallArgDef("pch",Lit("$"))
            )),
          FunDecl(
            List(DeclArg("m")),
            FunCall(
              Var("max"),
              List(
                CallArg(
                  FunCall(
                    Var("abs"),
                    List(CallArg(Var("m")))))
              ))),
          FunDecl(
            List(),
            FunCall(
              Var("max"),
              List(NoneArg))),
          Add(Num(_),Num(_)),
          _*
        )) => true
      case _ => false
    }
    assert(res)
  }

  test("new line delimiter bug fix test") {
    val input =
        """ 4
            8
            function() {
            4.8 + 6.0
            4 +  5
            }
            x
            """
    val res = RParser.parseUnwrap(input, RParser.rProgram) match {
      case Block(
        List(
          Num(_),
          Num(_),
          FunDecl(
            List(),
            Block(List(
              Add(Num(_),Num(_)),
              Add(Num(_),Num(_))))),
          Var("x"),
          _*
        )) => true
      case _ => false
    }
    assert(res)
  }
}