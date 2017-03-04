namespace YANGParser

open System.Text.RegularExpressions
open FParsec
open YANGParser.Ast

[<AutoOpen>]
module Parser =

    // An identifier can start with a letter or an underscore,
    // except X, M, or L
    let idStart c = (isAsciiLetter c || c = '_') && c <> 'X' && c <> 'x' && c <> 'M' && c <> 'm' && c <> 'L' && c <> 'l'
    let idContinue c = isAsciiLetter c || isDigit c || c = '_' || c = '-' || c = '.'
    let idPreCheckStart c = isAsciiLetter c || c = '_'

    let id : Parser<string, unit> =
        identifier (IdentifierOptions(isAsciiIdStart = idStart,
                                      isAsciiIdContinue = idContinue,
                                      preCheckStart = idPreCheckStart))

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

        let unquotedString = manySatisfy (isNoneOf " \t\r\n;{}")
        let quotString = (quotedString '"' true) <|> (quotedString '\'' false)
        
        // Quoted strings can be concatenated using the "+" operator
        let multipleQuotedStrings =
            sepBy1 quotString (spaces >>. pstring "+" .>> spaces)
            |>> String.concat ""

        let finalParser = multipleQuotedStrings <|> unquotedString

        // Returns a special parser that saves the column of the first char
        // and uses that to strip leading whitespace in multiline strings
        fun stream ->
            let col = stream.Column
            let reply = finalParser stream
            if reply.Status <> Ok then
                reply
            else
                Reply(stripWhitespaceFromStringLiteral reply.Result (int col))