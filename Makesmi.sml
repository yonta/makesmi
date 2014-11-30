structure MakeSmi
          : sig
            val main : string -> string
          end
=
struct

  fun bug s = Bug.Bug ("Top: " ^ s)

  open TopData

  val defaultOptions =
      {
        stopAt = NoStop,
        baseFilename = NONE,
        stdPath = nil,
        loadPath = nil
      } : options

  fun extendContext ({topEnv, fixEnv, version, builtinDecls} : toplevelContext,
                     {topEnv=newTopEnv, fixEnv=newFixEnv} : newContext) =
      let
        val topEnv = NameEvalEnv.topEnvWithTopEnv (topEnv, newTopEnv)
      in
        {topEnv = topEnv,
         version = version,
         fixEnv = Elaborator.extendFixEnv (fixEnv, newFixEnv),
         builtinDecls = builtinDecls} : toplevelContext
      end

  fun incVersion (context as {version, ...}: toplevelContext) =
      context # {version = case version of
                             NONE => SOME 0
                           | SOME i => SOME (i + 1)}

  val emptyNewContext =
      {
        topEnv = NameEvalEnv.emptyTopEnv,
        fixEnv = SEnv.empty
      } : newContext

  val errorOutput = TextIO.stdErr
  fun printError msg = TextIO.output (errorOutput, msg)
  fun flushError () = TextIO.flushOut errorOutput

  fun printLines title formatter elems =
      (if title = "" then () else (printError title; printError ":\n");
       app (fn elem =>
               let
                 val s = formatter elem
               in
                 if s <> "" then
                   (printError s; printError "\n")
                 else ()
               end
           ) elems;
       flushError ())

  fun userErrorToString x =
      Control.prettyPrint (UserError.format_errorInfo x)

  fun printWarnings warnings =
      if !Control.printWarning
      then printLines "" userErrorToString warnings
      else ()

  fun printDiagnosis title diagnoses =
      if !Control.printDiagnosis
      then printLines title userErrorToString diagnoses
      else ()

  fun printCode flag formatter title codes =
      if !flag
      then printLines title formatter codes
      else ()

  fun printParseResult flag title code =
      printCode flag AbsynFormatter.unitParseResultToString title [code]

  fun printAbsyn flag title code =
      printCode flag (Control.prettyPrint o AbsynInterface.format_compileUnit)
                title [code]

  fun printPatternCalc flag title code =
      printCode flag
                (Control.prettyPrint o PatternCalcInterface.format_compileUnit)
                title [code]

  fun printPatternCalcInteractive flag title code =
      printCode flag
                (Control.prettyPrint o PatternCalcInterface.format_interactiveUnit)
                title [code]

  fun printIDCalc flag title code =
      printCode
        flag
        (if !Control.printWithType
         then (Control.prettyPrint o IDCalc.formatWithType_icdecl)
         else (Control.prettyPrint o IDCalc.format_icdecl))
        title code

  fun printTypedCalc flag title code =
      printCode
        flag
        (if !Control.printWithType
         then Control.prettyPrint o (TypedCalc.formatWithType_tpdecl nil)
         else Control.prettyPrint o (TypedCalc.format_tpdecl nil))
        title code

  fun printRecordCalc flag title code =
      printCode flag
                (if !Control.printWithType
                 then RecordCalcFormatter.rcdecToString
                 else RecordCalcFormatter.rcdecToStringWithoutType)
                title code

  fun printTypedLambda flag title code =
      printCode flag
                (if !Control.printWithType
                 then TypedLambdaFormatter.tldecToStringWithType
                 else TypedLambdaFormatter.tldecToString)
                title code

  fun printBitmapCalc2 flag title code =
      printCode flag
                (if !Control.printWithType
                 then Control.prettyPrint o BitmapCalc2.formatWithType_bcdecl
                 else Control.prettyPrint o BitmapCalc2.format_bcdecl)
                title code

  fun printClosureCalc flag title code =
      printCode flag
                (if !Control.printWithType
                 then Control.prettyPrint o ClosureCalc.formatWithType_program
                 else Control.prettyPrint o ClosureCalc.format_program)
                title [code]

  fun printRuntimeCalc flag title code =
      printCode flag
                (if !Control.printWithType
                 then Control.prettyPrint o RuntimeCalc.formatWithType_program
                 else Control.prettyPrint o RuntimeCalc.format_program)
                title [code]

  fun printANormal flag title code =
      printCode flag
                (if !Control.printWithType
                 then Control.prettyPrint o ANormal.formatWithType_program
                 else Control.prettyPrint o ANormal.format_program)
                title [code]

  fun printMachineCode flag title code =
      printCode flag
                (Control.prettyPrint o MachineCode.format_program)
                title [code]

  fun printLLVMIR flag title code =
      printCode flag
                (Control.prettyPrint o LLVMIR.format_program)
                title [code]

  fun printLLVMModule flag title module =
      if !flag
      then (printError title; printError ":\n"; flushError ();
            LLVM.LLVMDumpModule module)
      else ()

  fun doParse input =
      let
        val _ = #start Counter.parseTimeCounter()
        val ret = Parser.parse input
        val _ =  #stop Counter.parseTimeCounter()
        val _ = printParseResult Control.printParse "Parsed" ret
      in
        case ret of
          Absyn.UNIT unit => unit
        | Absyn.EOF =>
          {interface = Absyn.NOINTERFACE, tops = nil, loc = Loc.noloc}
      end

  fun doLoadFile ({baseFilename, stdPath, loadPath, ...}:options)
                 ({version, ...}: toplevelContext)
                 absyn =
      let
        val _ = #start Counter.loadFileTimeCounter()
        val (dependency, abunit) =
            LoadFile.load
              {baseFilename=baseFilename, stdPath=stdPath, loadPath=loadPath}
              absyn
        val _ = #stop Counter.loadFileTimeCounter()
        val _ = printAbsyn Control.printLoadFile "File Loaded" abunit
      in
        (dependency, abunit)
      end

  fun provideName ({interface, ...} : AbsynInterface.compileUnit) =
      case interface of
        NONE => NONE
      | SOME {provideInterfaceNameOpt, ...} => provideInterfaceNameOpt

  fun doGenerateModuleName version abunit =
      let
        val _ = #start Counter.generateMainTimeCounter()
        val moduleName = GenerateMain.moduleName (provideName abunit, version)
        val _ =  #stop Counter.generateMainTimeCounter()
        val _ = if !Control.printGenerateMain
                then printError ("Generated module name: "
                                 ^ #moduleName moduleName ^ "\n")
                else ()
      in
        moduleName
      end

  fun doElaboration fixEnv abunit =
      let
        val _ = #start Counter.elaborationTimeCounter()
        val (newFixEnv, plunit, warnings) = Elaborator.elaborate fixEnv abunit
        val _ =  #stop Counter.elaborationTimeCounter()
        val _ = printWarnings warnings
        val _ = printPatternCalc Control.printElab "Elaborated" plunit
      in
        (newFixEnv, plunit)
      end

  fun doElaborationInteractiveEnv fixEnv interactiveUnit =
      let
        val _ = #start Counter.elaborationTimeCounter()
        val (newFixEnv, plinteractievUnit, warnings) =
            Elaborator.elaborateInteractiveEnv fixEnv interactiveUnit
        val _ =  #stop Counter.elaborationTimeCounter()
        val _ = printWarnings warnings
        val _ = printPatternCalcInteractive Control.printElab "Elaborated" plinteractievUnit
      in
        (newFixEnv, plinteractievUnit)
      end

  fun doNameEvaluation ({topEnv, version, builtinDecls, ...}:toplevelContext) plunit =
      let
        val _ = #start Counter.nameEvaluationTimeCounter()
        val (exnConList, nameevalTopEnv, icdecls, warnings) =
            NameEval.nameEval {topEnv=topEnv, version=version,
                               systemDecls=builtinDecls} plunit
        val _ =  #stop Counter.nameEvaluationTimeCounter()
        val _ = printWarnings warnings
        val _ = printIDCalc Control.printNameEval "Name Evaluation" icdecls
      in
        (exnConList, nameevalTopEnv, icdecls)
      end

  fun doNameEvaluationInteractiveEnv topEnv plinteractive =
      let
        val _ = #start Counter.nameEvaluationTimeCounter()
        val (nameevalTopEnv, warnings) =
            NameEval.nameEvalInteractiveEnv topEnv plinteractive
        val _ =  #stop Counter.nameEvaluationTimeCounter()
        val _ = printWarnings warnings
      in
        nameevalTopEnv
      end

  fun doTypeInference idcalc =
      let
        val _ = #start Counter.typeInferenceTimeCounter()
        val (typeinfVarE, tpdecs, warnings) = InferTypes.typeinf idcalc
        val _ =  #stop Counter.typeInferenceTimeCounter()
        val _ = printWarnings warnings
        val _ = printTypedCalc Control.printTypeInf "Type Inference" tpdecs
      in
        (typeinfVarE, tpdecs)
      end

  fun doPrinterGeneration exnConList topEnv tpdecs =
      let
        val _ = #start Counter.printerGenerationTimeCounter()
        val (topEnv, externDecls, printDecls) =
            PrinterGeneration.generate exnConList topEnv
        val tpdecs = externDecls @ tpdecs @ printDecls
        val _ =  #stop Counter.printerGenerationTimeCounter()
        val _ = printTypedCalc Control.printPrinterGen
                               "Printer Generated" tpdecs
      in
        tpdecs
      end

  fun doUncurryOptimization tpdecs =
      let
        val _ = #start Counter.UncurryOptimizationTimeCounter()
        val tpdecs = UncurryFundecl.optimize tpdecs
        val _ =  #stop Counter.UncurryOptimizationTimeCounter()
        val _ = printTypedCalc Control.printUncurryOpt
                               "Uncurrying Optimized" tpdecs
      in
        tpdecs
      end

  fun doTypedCalcOptimization tpdecs =
      let
        val _ = #start Counter.TypedCalcOptimizationTimeCounter()
        val tpdecs = TPOptimize.optimize tpdecs
        val _ =  #stop Counter.TypedCalcOptimizationTimeCounter()
        val _ = printTypedCalc Control.printTCOpt
                               "TypedCalc Optimized" tpdecs
      in
        tpdecs
      end

  fun doRecordCalcOptimization rcdecs =
      let
        val _ = #start Counter.RecordCalcOptimizationTimeCounter()
        val rcdecs = RCOptimize.optimize rcdecs
        val _ =  #stop Counter.RecordCalcOptimizationTimeCounter()
        val _ = printRecordCalc Control.printRCOpt
                                "RecordCalc Optimized" rcdecs
      in
        rcdecs
      end

  fun doVALRECOptimization iddecs =
      let
        val _ = #start Counter.valRecOptimizationTimeCounter()
        val iddecs = VALREC_Optimizer.optimize iddecs
        val _ =  #stop Counter.valRecOptimizationTimeCounter()
        val _ = printIDCalc Control.printVALRECOpt "VAL REC optimize" iddecs
      in
        iddecs
      end

  fun doFundeclElaboration iddecs =
      let
        val _ = #start Counter.fundeclElaborationTimeCounter()
        val iddecs = TransFundecl.transIcdeclList iddecs
        val _ =  #stop Counter.fundeclElaborationTimeCounter()
        val _ = printIDCalc Control.printFundeclElab
                            "Fundecl Elaboration" iddecs
      in
        iddecs
      end

  fun doMatchCompilation tpdecs =
      let
        val _ = #start Counter.matchCompilationTimeCounter()
        val (rcdecs, warnings) = MatchCompiler.compile tpdecs
        val _ =  #stop Counter.matchCompilationTimeCounter()
        val _ = printRecordCalc Control.printMatchCompile
                                "Match Compiled" rcdecs
        val _ = printWarnings warnings
      in
        rcdecs
      end

  fun doSQLCompilation icdecs =
      let
        val _ = #start Counter.sqlCompilationTimeCounter()
        val icdecs = SQLCompilation.compile icdecs
        val _ =  #stop Counter.sqlCompilationTimeCounter()
        val _ = printIDCalc Control.printSQLCompile "SQL Compiled" icdecs
      in
        icdecs
      end

  fun doFFICompilation rcdecs =
      let
        val _ = #start Counter.ffiCompilationTimeCounter()
        val rcdecs = FFICompilation.compile rcdecs
        val _ =  #stop Counter.ffiCompilationTimeCounter()
        val _ = printRecordCalc Control.printFFICompile "FFI Compiled" rcdecs
      in
        rcdecs
      end

  fun doRecordCompilation rcdecs =
      let
        val _ = #start Counter.recordCompilationTimeCounter()
        val rcdecs = RecordCompilation.compile rcdecs
        val _ =  #stop Counter.recordCompilationTimeCounter()
        val _ = printRecordCalc Control.printRecordCompile
                                "Record Compiled" rcdecs
      in
        rcdecs
      end

  fun doDatatypeCompilation rcdecs =
      let
        val _ = #start Counter.datatypeCompilationTimeCounter()
        val tldecs = DatatypeCompilation.compile rcdecs
        val _ =  #stop Counter.datatypeCompilationTimeCounter()
        val _ = printTypedLambda Control.printDatatypeCompile
                                 "Datatype Compiled" tldecs
      in
        tldecs
      end

  fun doBitmapCompilation2 tldecs =
      let
        val _ = #start Counter.bitmapCompilationTimeCounter()
        val bcdecs = BitmapCompilation2.compile tldecs
        val _ =  #stop Counter.bitmapCompilationTimeCounter()
        val _ = printBitmapCalc2 Control.printBitmapCompile
                                 "Bitmap Compiled" bcdecs
      in
        bcdecs
      end

  fun doClosureConversion2 bcexp =
      let
        val _ = #start Counter.closureConversionTimeCounter()
        val ccprog = ClosureConversion2.convert bcexp
        val _ = #stop Counter.closureConversionTimeCounter()
        val _ = printClosureCalc Control.printClosureConversion
                                 "Closure Converted" ccprog
      in
        ccprog
      end

  fun doCallingConventionCompile ccprog =
      let
        val _ = #start Counter.callingConventionCompileTimeCounter()
        val ncprog = CallingConventionCompile.compile ccprog
        val _ = #stop Counter.callingConventionCompileTimeCounter()
        val _ = printRuntimeCalc Control.printCConvCompile
                                 "Calling Convention Compiled" ncprog
      in
        ncprog
      end

  fun doANormalize ncprog =
      let
        val _ = #start Counter.anormalizeTimeCounter()
        val anprog = ANormalize.compile ncprog
        val _ = #stop Counter.anormalizeTimeCounter()
        val _ = printANormal Control.printANormal "A-normalized" anprog
        val _ = if !Control.checkType then ANormalTypeCheck.check anprog else ()
      in
        anprog
      end

  fun doMachineCodeGen moduleName anprog =
      let
        val _ = #start Counter.machineCodeGenTimeCounter()
        val mainSymbol = {mainSymbol = #mainSymbol moduleName}
        val mcprog = MachineCodeGen.compile mainSymbol anprog
        val _ = #stop Counter.machineCodeGenTimeCounter()
        val _ = printMachineCode Control.printMachineCodeGen
                                 "MachineCodeGen" mcprog
      in
        mcprog
      end

  fun doInsertCheckGC mcprog =
      let
        val _ = #start Counter.insertCheckGCTimeCounter()
        val mcprog = ConcurrencySupport.insertCheckGC mcprog
        val _ = #stop Counter.insertCheckGCTimeCounter()
        val _ = printMachineCode Control.printInsertCheckGC
                                 "Insert Check GC" mcprog
      in
        mcprog
      end

  fun doStackAllocation mcprog =
      let
        val _ = #start Counter.stackAllocationTimeCounter()
        val mcprog = StackAllocation.compile mcprog
        val _ = #stop Counter.stackAllocationTimeCounter()
        val _ = printMachineCode Control.printStackAllocation
                                 "Stack Allocation" mcprog
      in
        mcprog
      end

  fun doLLVMGen moduleName mcprog =
      let
        val _ = #start Counter.llvmGenTimeCounter()
        val moduleName = {moduleName = #moduleName moduleName}
        val llvmprog = LLVMGen.compile moduleName mcprog
        val _ = #stop Counter.llvmGenTimeCounter()
        val _ = printLLVMIR Control.printLLVMGen "LLVM generated" llvmprog
      in
        llvmprog
      end

  fun doLLVMEmit llvmir =
      let
        val _ = #start Counter.llvmEmitTimeCounter()
        val module = LLVMEmit.emit llvmir
        val _ = #stop Counter.llvmEmitTimeCounter()
        val _ = printLLVMModule Control.printLLVMEmit "LLVM Emit" module
      in
        module
      end

(*
  fun writeLLVMIR (dstfile, module) =
      (#start Counter.llvmOutputTimeCounter();
       LLVM.LLVMPrintModuleToFile (module, Filename.toString dstfile);
       #stop Counter.llvmOutputTimeCounter())

  fun writeLLVMBitcode (dstfile, module) =
      (#start Counter.llvmOutputTimeCounter();
       LLVM.LLVMWriteBitcodeToFile (module, Filename.toString dstfile);
       #stop Counter.llvmOutputTimeCounter())

  type write_native_options =
       {optLevel: LLVM.CodeGenOptLevel,
        relocModel: LLVM.RelocModel,
        codeModel: LLVM.CodeModel,
        arch: string option}

  local
    fun emitToFile ({optLevel, relocModel, codeModel, arch}
                    :write_native_options)
                   (dstfile, module) filetype =
        (
          #start Counter.llvmOutputTimeCounter();
          LLVM.compile {module = module,
                        arch = case arch of SOME x => x | NONE => "",
                        cpu = "",
                        optLevel = optLevel,
                        relocModel = relocModel,
                        codeModel = codeModel,
                        fileType = filetype,
                        outputFilename = Filename.toString dstfile};
          #stop Counter.llvmOutputTimeCounter()
        )
  in

  fun writeNativeAssembly options module =
      emitToFile options module LLVM.CGFT_AssemblyFile

  fun writeNativeObject options module =
      emitToFile options module LLVM.CGFT_ObjectFile

  end (* local *)
*)

  exception Return of LoadFile.dependency * result

 fun compile (options as {stopAt,baseFilename,stdPath,loadPath})
              (context as {topEnv, fixEnv, version, builtinDecls}) input =
      let
        val _ = #start Counter.compilationTimeCounter()

        val parsed = doParse input
        val (dependency, abunit) = doLoadFile options context parsed
        val moduleName = doGenerateModuleName version abunit

        val (newFixEnv, plunit) = doElaboration fixEnv abunit

        val _ = if stopAt = SyntaxCheck
                then raise Return (dependency, STOPPED)
                else ()

        val (exnConList, nameevalTopEnv, idcalc) = doNameEvaluation context plunit
        val idcalc = doSQLCompilation idcalc
        val idcalc = doVALRECOptimization idcalc

        val idcalc = if !Control.doUncurryOptimization
                     then idcalc
                     else doFundeclElaboration idcalc

        val (typeinfVarEnv, tpcalc) =
            doTypeInference idcalc handle exn => raise exn
      in
        tpcalc
      end

  fun evalMain (env:env) mainExp =
      case mainExp of
        | CompileSML (options, context, filename) =>
        let
          val io = Filename.TextIO.openIn filename
          val (depends, result) =
              Top.compile options context
                          (Parser.setup
                             {mode = Parser.File,
                              read = fn (_,n) => TextIO.inputN (io, n),
                              sourceName = Filename.toString filename,
                              initialLineno = 1})
              handle e => (TextIO.closeIn io; raise e)
          val _ = TextIO.closeIn io
          val code =
              case result of
                Top.RETURN (_, module) => SOME module
              | Top.STOPPED => NONE
        in
          SML (depends, code)
        end

  fun main file =
      let
        val options = hoge
        val context = fuga
        val parserInput = puyo
        val tpcalc = compile options context parserInput
      in
        map Printer.printTpdecl tpcalc
      (* uwaaaaaaaaaaaaaaaaaaaaaaaaaaaa *)
      end



  type env = {vars: mainResult list, printOut: TextIO.outstream}

  fun main progname =
      let
        val args = nil
        val mainExp = compileArgs (progname, args)
        val emptyEnv = {vars = nil, printOut = TextIO.stdOut} : env
        val result = evalMain emptyEnv mainExp
      in
        case result of
          SUCCESS => ()
        | _ => raise Bug.Bug "main: result is not SUCCESS";
        if !Control.doProfile
        then (print "Time Profile:\n"; print (Counter.dump ())) else ();
        TempFile.cleanup ();
        OS.Process.success
      end
      handle e =>
        (
          TempFile.cleanup ();
          printExn progname e;
          OS.Process.failure
        )


end
