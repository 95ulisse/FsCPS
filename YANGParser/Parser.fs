namespace YANGParser

open FParsec

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