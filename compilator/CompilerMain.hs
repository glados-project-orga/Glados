import AST

module CompilerMain where

compileStruct :: Declaration -> ConstantPool -> Bytecode -> (ConstantPool, Bytecode)
compileStruct _ _ _ = ([], [])

compileFunction :: Declaration -> ConstantPool -> Bytecode -> (ConstantPool, Bytecode)
compileFunction _ _ _ = ([], [])

compileEnum :: Declaration -> ConstantPool -> Bytecode -> (ConstantPool, Bytecode)
compileEnum _ _ _ = ([], [])



compileDeclarations :: Ast -> (ConstantPool, Bytecode)
compileDeclarations [] constPool bytecode = ([], [])
compileDeclaration (fun@(FunctionDecl _ _ _ _ _):ast) constPool bytecode =
    compileFunction fun ++ compileDeclarations ast
compileDeclaration (struct@(StructDecl _ _ _):ast) constPool bytecode =
    compileStruct struct ++ compileDeclarations ast
compileDeclaration (enum@(EnumDecl _ _):ast) constPool bytecode =
    compileEnum enum ++ compileDeclarations ast
compileDeclaration (typedef@(TypedefDecl _ _ _):ast) constPool bytecode =
    compileTypedef typedef ++ compileDeclarations ast

compilerMain :: Ast -> (ConstantPool, Bytecode)
compilerMain ast = compileDeclarations (ast)