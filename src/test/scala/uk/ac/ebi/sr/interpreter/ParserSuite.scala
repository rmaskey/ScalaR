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

  test(" analytics parse test") {
    assert(RParser.parseUnwrap(analytics).isInstanceOf[Expression])
  }
  
  val analytics =
  """(function() {

### Reads data from Atlas NetCDF file and returns an ExpressionSet
### Post-AEW migration TODOs:
### * NChannelSet
### * Multiple array designs
read.atlas.nc <<-
  function (filename, accnum=NULL) {
    require(ncdf)
    require(Biobase)
    require(gtools)

    nc = open.ncdf(filename)

    bdc = get.var.ncdf(nc,"BDC")
    as  = get.var.ncdf(nc,"AS")
    bs  = get.var.ncdf(nc,"BS")
    b2a = get.var.ncdf(nc,"BS2AS")

    ef  = get.var.ncdf(nc,"EF")
    efv = get.var.ncdf(nc,"EFV")
    sc  = get.var.ncdf(nc,"SC")
    scv = get.var.ncdf(nc,"SCV")

    de  = get.var.ncdf(nc,"DE")
    # deacc = get.var.ncdf(nc,"DEacc")
    gn  = get.var.ncdf(nc,"GN")

    # make de's unique
    de[de==0] <- -(1:length(which(de==0)))


    accnum = att.get.ncdf(nc,varid=0,"experiment_accession")$value
    qt     = att.get.ncdf(nc,varid=0,"quantitationType")$value
    adacc  = att.get.ncdf(nc,varid=0,"ADaccession")$value
    adname = att.get.ncdf(nc,varid=0,"ADname")$value

    close.ncdf(nc)

    colnames(efv) = ef
    rownames(efv) = as
    efv = data.frame(efv)

    colnames(scv) = sc
    rownames(scv) = bs
    scv = data.frame(scv)

    for ( i in 1:length(ef) ) {
      lefv <- levels(efv[[i]])
      if(length(lefv) > 1) {
        levels(efv[[i]]) = mixedsort(lefv)
      }
    }

    for ( i in 1:length(sc) ) {
      lscv = levels(scv[[i]])
      if(length(lscv) > 1) {
        levels(scv[[i]]) = mixedsort(lscv)
      }
    }

    bdc[bdc<=-1e6] = NA

	if(length(as)==1) {
		bdc = matrix(bdc, nrow=length(de))
		b2a = matrix(b2a, nrow=1)
	} else {
    	bdc = t(bdc)
    }
    rownames(bdc) = de
    colnames(bdc) = as

    rownames(b2a) = as
    colnames(b2a) = bs

    print(paste("Read in", accnum))
    print(paste("Read in BDC:",nrow(bdc),"x",ncol(bdc)))
    print(paste("Read in EFV:",nrow(efv),"x",ncol(efv)))
    print(paste("Read in SCV:",nrow(scv),"x",ncol(scv)))

    ncinfo = unlist(strsplit(basename(filename),"_|[.]"))
    exptid = ncinfo[1]
    arraydesignid = ncinfo[2]

    efscv <- efv
#   MK: remove this for now, to prevent analytics running on SC's
#    for(sc in colnames(scv)) {
#      scvj <- as.factor(unlist(lapply(rownames(efv), function(ef)
#                                      paste(unique(scv[colnames(b2a)[as.logical(b2a[ef,])],sc]),
#                                            ## colnames(b2a)[as.logical(b2a[ef,])],
#                                            sep=":", collapse="|"))))
#
#      ef <- sub("bs_","ba_",sc)
#      if( !identical(efscv[[ef]], scvj)) {
#        efscv[[sc]] <- scvj
#      }
#    }

    fDataFrame = data.frame(gn=gn,de=de) #, deacc=deacc)
    fData = new("AnnotatedDataFrame", data=fDataFrame)
    featureNames(fData) = de
    pData  = new("AnnotatedDataFrame", data=efscv)
    scData = new("AnnotatedDataFrame", data=scv)
    eData = new("MIAME",
      other=list(accession=accnum,
        experimentid=exptid,
        arraydesignid=arraydesignid,
        qt=qt,
        arraydesignaccnum=adacc,
        arraydesignname=adname
        )
      )

    attr(eData, "scv") <- scv
    attr(eData, "b2a") <- b2a

    aData <- assayDataNew(storage.mode="lockedEnvironment", exprs=bdc)
    featureNames(aData) <- de
    sampleNames(aData) <- as

    print(paste("Computed phenoData for", paste(varLabels(pData),collapse=", "), nrow(pData), "x", ncol(pData)))
    return(new("ExpressionSet", assayData=aData, phenoData=pData, featureData=fData, experimentData=eData))
  }

### Log function that does not return NAs
log2.safe <- function(x,log=T) {
  if(!log)
    return(x)
  tmp=log2(x)
  tmp[!is.finite(tmp)]=0
  tmp
}

### Omnibus one-way ANOVA (with moderated t) F-statistic computation
fstat.eset <- function(eset, design=NULL, varLabel=NULL,lg=FALSE) {
  if(is.null(design) && !is.null(varLabel)) {
    print(paste("Calculating design matrix for", varLabel))
    design = model.matrix(as.formula(paste("~0+", varLabel)), data=eset)
  }

  if(lg==TRUE) {
    exprs(eset)<-log2.safe(exprs(eset))
  }

  print("Fitting model...")
  fit=lmFit(eset,design)

#  print("Re-fitting model to ANOVA contrasts...")
#  pairs=design.pairs(colnames(design))
#  cfit=contrasts.fit(fit,pairs)

#  print("Moderating...")
#  cfit=eBayes(cfit)
   fit=eBayes(fit)

#  fit$F=cfit$F
#  fit$F.p.value=cfit$F.p.value

  return(fit)
}

### Moderated t one-way ANOVA post-hoc testing with global FDR adjustment
allupdn <- function (eset, alpha=0.01, evars=varLabels(eset) ) {
  require(limma)

  exprs = exprs(eset)
  if(max(exprs,na.rm=TRUE)>1000 && min(exprs,na.rm=TRUE	)>=0) {
    print("Taking log2 of the expression matrix")
    exprs(eset) = log2.safe(exprs)
  }

  allFits = list()

  for(varLabel in evars){
    try({
      print(paste("Calculating lmFit and F-stats for", varLabel))
      if( length(levels(eset[[varLabel, exact=TRUE]]))<2 ||
        length(levels(eset[[varLabel, exact=TRUE]]))>200 ||
        length(levels(eset[[varLabel, exact=TRUE]])) == ncol(exprs) ) { next }
      thisFit = fstat.eset(eset,varLabel=varLabel)

      print("Adjusting p-values")
#      pp = p.adjust(thisFit$F.p.value,method="fdr")
#      w=which(pp<=alpha)
#
#      thisFit$F.p.value.adj=pp

      n = ncol(thisFit$design)
      cm = diag(n)-1/n

      contr.fit=contrasts.fit(thisFit,cm)
      contr.fit=eBayes(contr.fit)

      dec = decideTests(contr.fit,method="global", adjust.method="fdr")
      colnames(dec) = levels(eset[[varLabel, exact=TRUE]])

#      thisFit$which=w
      thisFit$boolupdn=dec
      thisFit$contr.fit=contr.fit

      allFits[[varLabel]] = thisFit
      print("Done.")
    })
  }

  allFits
}

### Atlas analytics processing driver: read the data, compute the linear fit, post-hoc test, adjust and write to tab-delim files
process.atlas.nc<-
  function (nc)
{
  eset = read.atlas.nc(nc)
  info = otherInfo(experimentData(eset))
  proc = allupdn(eset)

  print("Writing out the results")
  for(varLabel in varLabels(eset)) {
    if(!is.null(proc[[varLabel, exact=TRUE]]$contr.fit)) {
      fitfile <-  paste(info$accession,"_",info$experimentid,"_",info$arraydesignid,"_",varLabel,"_","fit.tab",sep="")
      tab <- list()
      tab$A <- proc[[varLabel, exact=TRUE]]$Amean
                                        #		    tab$Coef <- proc[[varLabel, exact=TRUE]]$contr.fit$coef
      tab$t <- proc[[varLabel, exact=TRUE]]$contr.fit$t
      tab$p.value <- as.matrix(proc[[varLabel, exact=TRUE]]$contr.fit$p.value)

      pv = tab$p.value
      o = !is.na(tab$p.value)
      pv[o] = p.adjust(pv[o], method="fdr")

      tab$p.value.adj = pv
      tab$Res <- unclass(proc[[varLabel, exact=TRUE]]$boolupdn)
      tab$F <- proc[[varLabel, exact=TRUE]]$F
      tab$F.p.value <- proc[[varLabel, exact=TRUE]]$F.p.value
      tab$F.p.value.adj = proc[[varLabel, exact=TRUE]]$F.p.value.adj
      tab$Genes <- proc[[varLabel, exact=TRUE]]$genes
      tab <- data.frame(tab, check.names = FALSE)
      write.table(tab, file = fitfile, quote = FALSE, row.names = FALSE, sep = "\t")
      print(paste("Wrote",fitfile))
    }
  }
}

### Atlas analytics, returns instead of writing
computeAnalytics <<-
function (nc)
{
  e <- try({
    eset = read.atlas.nc(nc)
    ncd  = open.ncdf(nc, write=TRUE)

    if(dim(eset)[2] == 1) {
        return(sapply(varLabels(eset),function(i) "NOK"))
    }

    proc = allupdn(eset)

    uEFV  = get.var.ncdf(ncd, "uEFV")
    tstat = t(get.var.ncdf(ncd, "TSTAT"))
    pval  = t(get.var.ncdf(ncd, "PVAL"))

    colnames(tstat) <- make.names(uEFV)
    colnames(pval)  <- make.names(uEFV)

    result <- sapply(varLabels(eset), function(varLabel) {
      print(paste("Processing",varLabel))
      if(!is.null(proc[[varLabel, exact=TRUE]]$contr.fit)) {
        tab <- list()
        tab$A <- proc[[varLabel, exact=TRUE]]$Amean
        tab$t <- proc[[varLabel, exact=TRUE]]$contr.fit$t
        tab$p.value <- as.matrix(proc[[varLabel, exact=TRUE]]$contr.fit$p.value)

        pv = tab$p.value
        o = !is.na(tab$p.value)
        pv[o] = p.adjust(pv[o], method="fdr")

        tab$p.value.adj = pv
        tab$Res <- unclass(proc[[varLabel, exact=TRUE]]$boolupdn)
#        tab$F <- proc[[varLabel, exact=TRUE]]$F
#        tab$F.p.value <- proc[[varLabel, exact=TRUE]]$F.p.value
#        tab$F.p.value.adj = proc[[varLabel, exact=TRUE]]$F.p.value.adj
        tab$Genes <- proc[[varLabel, exact=TRUE]]$genes

        colnames(tab$Res) <- make.names(paste(varLabel,colnames(tab$Res),sep="||"))

        colnames(tab$t)           <- colnames(tab$Res)
        colnames(tab$p.value.adj) <- colnames(tab$Res)

        tstat[,which(colnames(tstat) %in% colnames(tab$t))] <<- tab$t[,colnames(tstat)[which(colnames(tstat) %in% colnames(tab$t))] ]
        pval[,which(colnames(pval) %in% colnames(tab$p.value.adj))] <<- tab$p.value.adj[,colnames(pval)[which(colnames(pval) %in% colnames(tab$p.value.adj))] ]

        return("OK")
      } else {
        return("NOK")
      }
    })

    print("Writing tstat and pval to NetCDF")
    put.var.ncdf(ncd, "TSTAT", tstat)
    put.var.ncdf(ncd, "PVAL",  pval)

    ef  = get.var.ncdf(ncd,"EF")

    close.ncdf(ncd)

    names(result) <- ef
    return(result)
  })

  return(e)
}

### Compute a design matrix for making all possible pairwise comparisons (one-way ANOVA F).
design.pairs <- function(levels) {
  makeContrasts(contrasts=combn(levels, 2, paste, collapse = '-'),levels=levels)
}

})()

  """
}