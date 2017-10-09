namespace FsCPS.Yang

open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Globalization
open System.Text
open System.Text.RegularExpressions
open FParsec

/// Position in a file in terms of line and column.
[<StructuredFormatDisplay("{Line}:{Column}")>]
type Position = {
    Line: int64;
    Column: int64;
}

/// Generic statement.
type Statement(name: string, pos: Position) as this =
    member val Name = name
    member val Argument: string option = None with get, set
    member val Children = StatementCollection(this)
    member val Parent: Statement option = None with get, set
    member val Position: Position = pos

    /// Returns the string representation of this statement.
    override this.ToString() =
        let s = StringBuilder()
        
        s.Append(this.Name) |> ignore

        match this.Argument with
        | Some(x) -> s.Append(" \"").Append(x).Append("\"") |> ignore
        | None -> ()

        if this.Children.Count = 0 then
            s.Append(";") |> ignore
        else
            s.AppendLine(" {") |> ignore
            this.Children |> Seq.iter (fun child -> s.AppendLine(child.ToString()) |> ignore)
            s.AppendLine("}") |> ignore

        s.ToString()

and StatementCollection(owner: Statement) =
    inherit Collection<Statement>()

    override this.InsertItem(index: int, stmt: Statement) =
        if stmt.Parent.IsSome then
            raise (ArgumentException "Statement already has a parent.")
        stmt.Parent <- Some(owner)
        base.InsertItem(index, stmt)

    override this.SetItem(index: int, stmt: Statement) =
        if stmt.Parent.IsSome then
            raise (ArgumentException "Statement already has a parent.")
        this.[index].Parent <- None
        stmt.Parent <- Some(owner)
        base.SetItem(index, stmt)

    override this.RemoveItem(index: int) =
        this.[index].Parent <- None
        base.RemoveItem(index)

    override this.ClearItems() =
        this |> Seq.iter (fun x -> x.Parent <- None)
        base.ClearItems()


/// Generic range record.
type Range<'T> = {
    Min: 'T;
    Max: 'T;
}

/// A node path is a path to a node in the data tree. 
type NodePath =
    {
        IsAbsolute: bool;
        Segments: (string option * string) list
    }


module internal StatementParsers =

    // Whitespace and comments:
    // comments are C-like ("//" for a line comment and "/*" + "*/" for a block comment).
    // Note also that comments are considered as whitespace.

    let singleSpaceChar = skipAnyOf " \t\n\r"
    let lineComment = skipString "//" >>. skipRestOfLine true
    let blockComment =
        between (skipString "/*") (skipString "*/")
                (skipCharsTillString "*/" false System.Int32.MaxValue)

    let singleWhitespace = lineComment <|> blockComment <|> singleSpaceChar

    // `ws` parses optional whitespace, `ws1` parses at least one whitespace,
    // and fails if none has been found. Note that more consecutive whitespaces
    // are parsed as one.
    let ws: Parser<unit, unit> = skipMany singleWhitespace <?> "whitespace"
    let ws1: Parser<unit, unit> = skipMany1 singleWhitespace <?> "whitespace"
    let ws_nocomments: Parser<unit, unit> = skipMany singleSpaceChar <?> "whitespace"

    // ---------------------------------------------------------------------------

    // Helper function to strip whitespace from a multiline string literal.
    // Strips whitespace at the end of each line and also
    // skips initial spaces at the beginning of each line until the given column.
    // Tab chars "\t" are counted as 8 spaces.
    let trailingWhitespaceRegex = Regex("\\s*(\\r\\n?|\\n)", RegexOptions.Compiled)
    let leadingWhitespaceRegex = Regex("\\n\\s*", RegexOptions.Compiled)
    let stripWhitespaceFromStringLiteral str col =
        let s = trailingWhitespaceRegex.Replace(str, "\n")
        leadingWhitespaceRegex.Replace(s, (fun m ->
            // Checks that some space has been matched
            if m.Value.Length > 1 then
            
                // Count the total number of spaces: tabs count for 8,
                // and -1 is because the string contains a newline
                let spacesCount =
                    m.Value.Length - 1 +
                    7 * (m.Value |> Seq.fold (fun count c -> if c = '\t' then count + 1 else count) 0)
            
                // Replace all these spaces with the correct number
                if col > spacesCount then
                    "\n"
                else
                    "\n" + (String.replicate (spacesCount - col) " ")

            else
                m.Value
        ))

    // Creates a parses for a literal string quoted with the given character.
    // If `containsEscapes` is true, the stirng literal is allowed to have
    // escaped chars in it, and the sequences of chars "\" "n", "\" "r" and "\" "t"
    // will be parsed as a single char.
    let quotedString quote containsEscapes : Parser<string, unit> =
    
        let normalChar =
            if containsEscapes then
                manySatisfy (fun c -> c <> quote && c <> '\\')
            else
                manySatisfy (fun c -> c <> quote)

        let escapedChar =
            pstring "\\" >>. (anyOf "\\\"nrt" |>> function
                                                    | 'n' -> "\n"
                                                    | 'r' -> "\r"
                                                    | 't' -> "\t"
                                                    | c -> string c)

        if containsEscapes then
            between (pchar quote) (pchar quote)
                    (stringsSepBy normalChar escapedChar)
        else
            between (pchar quote) (pchar quote)
                    normalChar

    // String literal: can either be quoted or unquoted
    let stringLiteral: Parser<string, unit> =

        // Unquoted strings end with a whitespace (comments included) or
        // one of the following characters: ";", "{", "}"
        let unquotedString =
            many1CharsTill
                (satisfy (isNoneOf ";{} \t\n\r"))
                (followedBy (ws1 <|> (skipSatisfy (isAnyOf ";{}")) <|> eof))
    
        // Quoted strings can be concatenated using the "+" operator
        let quotString = (quotedString '"' true) <|> (quotedString '\'' false)
        let multipleQuotedStrings =
            sepBy1 quotString (ws >>? pstring "+" .>>? ws)
            |>> String.concat ""

        let finalParser = multipleQuotedStrings <|> unquotedString

        // Returns a special parser that saves the column of the first char
        // and uses that to strip leading whitespace in multiline strings
        (fun stream ->
            let col = stream.Column
            let reply = finalParser stream
            if reply.Status <> Ok then
                reply
            else
                Reply(stripWhitespaceFromStringLiteral reply.Result (int col))
        )
        <??> "string"

    // ---------------------------------------------------------------------------

    // An identifier can start with a letter or an underscore
    let idStart c = isAsciiLetter c || c = '_'
    let idContinue c = isAsciiLetter c || isDigit c || c = '_' || c = '-' || c = '.'

    let id : Parser<string, unit> =
        identifier (IdentifierOptions(isAsciiIdStart = idStart,
                                      isAsciiIdContinue = idContinue))
        <?> "identifier"

    // Keywords are just valid identifiers
    let keyword : Parser<string, unit> = id <?> "keyword"

    // ---------------------------------------------------------------------------

    // Statements
    let statement, statementRef = createParserForwardedToRef<Statement, unit> ()
    statementRef :=
        getPosition
        .>>. keyword                     // Name
        .>>. opt (ws1 >>? stringLiteral) // Argument
        .>>  ws
        .>>. (
            (pstring ";" |>> fun _ -> [])                           // No sub-statements
            <|>
            (pstring "{" >>. ws >>. many statement .>> pstring "}") // Sub-statements
        )
        .>> ws
        <??> "statement"
        |>> fun (((pos, name), arg), children) ->
                let s = Statement(
                            name,
                            { Line = pos.Line; Column = pos.Column },
                            Argument = arg
                        )
                children |> List.iter s.Children.Add
                s

    // Entry rule
    let root = ws >>. statement

    // ---------------------------------------------------------------------------

    // Other parsers used for argument parsing.
    // Note that this parsers enforce that the whole string must be parsed
    // (the final `eof`).

    let uintLength len : Parser<int, unit> =
        manyMinMaxSatisfy len len isDigit
        >>= (fun str ->
            match UInt32.TryParse(str) with
            | (true, i) -> preturn (int i)
            | _ -> fail "Invalid number"
        )

    let dateArg: Parser<_, unit> =
        ws_nocomments
         >>. uintLength 4
        .>>  skipChar '-'
        .>>. uintLength 2
        .>>  skipChar '-'
        .>>. uintLength 2
        .>>  ws_nocomments
        .>>  eof
        <?> "date"
        >>= (fun ((year, month), day) ->
            try
                preturn (DateTime(year, month, day))
            with
            | :? ArgumentException ->
                fail "Invalid date"
        )

    let ref: Parser<_, unit> =
        ws_nocomments
         >>. opt (id .>>? pstring ":")
        .>>. id
        .>>  ws_nocomments
        .>>  eof

    let range pnumber min max =
        let rangeBoundary =
            choice [
                skipString "min" |>> (fun _ -> min);
                skipString "max" |>> (fun _ -> max);
                pnumber;
            ]
        let rangePart =
            rangeBoundary
            .>>. opt (ws_nocomments >>? skipString ".." >>? ws_nocomments >>? rangeBoundary)
            |>> (fun (min, max) ->
                match max with
                | Some(actualMax) -> { Min = min; Max = actualMax }
                | None -> { Min = min; Max = min }
            )
        ws_nocomments
         >>. sepBy1 rangePart (ws_nocomments >>. skipChar '|' >>. ws_nocomments)
        .>>  ws_nocomments
        .>>  eof

    let lengthRangeDescription =
        range puint32 0u UInt32.MaxValue

    let rangeDescription =
        let numberFormat =
                NumberLiteralOptions.AllowMinusSign
            ||| NumberLiteralOptions.AllowPlusSign
            ||| NumberLiteralOptions.AllowFraction
    
        // The number parser is taken from the FParsec source, and simplified a bit.
        // The problem is that the `numberLiteral` parser allows number such as "1."
        // to be parsed successfully. We must remove the trailing dot, otherwise it
        // will cause problems with the range separator "..".
        let pnumber =
            fun stream ->
                let reply = numberLiteral numberFormat "number" stream
                if reply.Status = Ok then
                    let nl = reply.Result
                    try
                        let n = Double.Parse(nl.String, CultureInfo.InvariantCulture)

                        // The number is a valid double, so check if we need to backtrack the stream
                        // of just one char to compensate for the trailing dot.
                        if nl.String.EndsWith(".") then
                            stream.Skip(-1)

                        Reply(n)
                    with e ->
                        let error = if   (e :? System.OverflowException) then messageError "Number outside of the Double range."
                                    elif (e :? System.FormatException) then messageError "The floating-point number has an invalid format (this error is unexpected, please report this error message to fparsec@quanttec.com)."
                                    else reraise()
                        stream.Skip(-nl.String.Length)
                        Reply(FatalError, error)
                else
                    Reply(reply.Status, reply.Error)

        range pnumber Double.MinValue Double.MaxValue

    let nodePath =
        ws_nocomments
         >>. opt (skipChar '/')
        .>>. sepBy1 (opt (id .>>? skipChar ':') .>>. id) (skipChar '/')
        .>>  ws_nocomments
        .>>  eof
        |>> (fun (beginSlash, segments) -> { IsAbsolute = beginSlash.IsSome; Segments = segments })