namespace YANGParser

[<AutoOpen>]
module Ast =

    // Generic statement
    type Statement = {
        ns: string option;
        name: string;
        argument: string option;
        children: Statement list;
    }