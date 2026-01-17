module CompilerError (
    errPos
    ) where
import Ast (SourcePos(..))

errPos :: SourcePos -> String
errPos (SourcePos line col) =
    "Error at line " ++ show line ++ ", column " ++ show col ++ ": "