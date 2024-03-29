let
    val fileName =
        case CommandLine.arguments() of
            [x] => x
          | _ => raise Fail "Wrong Arguments"
    val fileStream = TextIO.openIn fileName handle Io =>
                                                   (TextIO.print ("Can't open "^fileName^".\n");
                                                    raise Fail "Exiting\n")
    val ast = Parser.parse fileStream
    val _ = print ((DslAst.layout ast) ^ "\n")
    val ast = CpsTrans.cpsTrans ast
    val _ = print ((DslAst.layout ast) ^ "\n")
in
    ()
end;
