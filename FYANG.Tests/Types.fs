namespace FYANG.Tests

open System
open System.Globalization
open Xunit
open FsCheck
open FsCheck.Xunit
open FYANG
open FYANG.Statements
open FYANG.Model

// Integer used to represent the value of the `fraction-digits` property
type FractionDigits = FractionDigits of int

// Finite float value
type FiniteFloat = FiniteFloat of float


type Arbitraries =
    static member FractionDigits() =
        Gen.elements [1..18]
        |> Gen.map FractionDigits
        |> Arb.fromGen
    static member FiniteFloat() =
        Arb.from<float>
        |> Arb.filter (fun x -> not (Double.IsInfinity(x) || Double.IsNaN(x)))
        |> Arb.convert FiniteFloat (fun (FiniteFloat x) -> x)


module TypeRestrictions =
    
    [<Property>]
    let ``RangeRestriction allows only values between the ranges`` (n: float) (r: Range<float> list) =
        let isInRange =
            r |> List.exists (fun rr -> n >= rr.Min && n <= rr.Max)
        YANGRangeRestriction(r).IsValid(n) = isInRange
    
    [<Property>]
    let ``LengthRestriction restricts on string length`` (s: string) (r: Range<uint32> list) =
        let ok =
            if isNull s then
                false
            else
                r
                |> List.exists (fun rr -> uint32 s.Length >= rr.Min && uint32 s.Length <= rr.Max)
        YANGLengthRestriction(r).IsValid(s) = ok

    [<Property>]
    let ``LengthRestriction restricts on byte array length`` (arr: byte[]) (r: Range<uint32> list) =
        let ok =
            if isNull arr then
                false
            else
                r
                |> List.exists (fun rr -> uint32 arr.Length >= rr.Min && uint32 arr.Length <= rr.Max)
        YANGLengthRestriction(r).IsValid(arr) = ok


[<Properties(Arbitrary = [| typeof<Arbitraries> |])>]
module Decimal64 =

    let makeDecimal64TypeNoDigits() =
        YANGType(
            { Namespace = YANGNamespace.Default; Name = "test-decimal64" },
            BaseType = Some YANGPrimitiveTypes.Decimal64
        )

    let makeDecimal64Type (FractionDigits digits) =
        let t = YANGType(
                    { Namespace = YANGNamespace.Default; Name = "test-decimal64" },
                    BaseType = Some YANGPrimitiveTypes.Decimal64
                )
        t.SetProperty("fraction-digits", digits)
        t

    let isInDecimal64Range (FractionDigits digits) n =
        let minRange = (float Int64.MinValue) / (10.0 ** float digits)
        let maxRange = (float Int64.MaxValue) / (10.0 ** float digits)
        n >= minRange && n <= maxRange

    [<Fact>]
    let ``Requires property fraction-digits`` () =
        let t = makeDecimal64TypeNoDigits()
        Assert.Throws<ArgumentException>(fun () -> t.Parse("1.0") |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun () -> t.Serialize(1.0) |> ignore) |> ignore
        t.SetProperty("fraction-digits", 1)
        t.Parse("1.0") |> ignore
        t.Serialize(1.0) |> ignore


    [<Property>]
    let ``Requires fraction-digits to be between 1 and 18`` (digits: int) =
        let t = makeDecimal64TypeNoDigits()
        t.SetProperty("fraction-digits", digits)
        if digits >= 1 && digits <= 18 then
            t.Parse("1.0").IsSome
        else
            Assert.Throws<ArgumentException>(fun () -> t.Parse("1.0") |> ignore) |> ignore
            true

    [<Property>]
    let ``Does not parse or serialize integers`` (digits: FractionDigits) (n: int) =
        let t = makeDecimal64Type digits

        t.Parse(n.ToString()).IsNone  |@ "Parse integer" .&.
        t.Serialize(n).IsNone         |@ "Serialize integer"

    [<Property>]
    let ``Does not parse or serialize NaN or Infinity`` (digits: FractionDigits) =
        let t = makeDecimal64Type digits

        t.Serialize(Double.NaN).IsNone                      |@ "Serialize NaN" .&.
        t.Serialize(Double.NegativeInfinity).IsNone         |@ "Serialize -Inf" .&.
        t.Serialize(Double.PositiveInfinity).IsNone         |@ "Serialize +Inf" .&.
        t.Parse(Double.NaN.ToString()).IsNone               |@ "Parse NaN" .&.
        t.Parse(Double.NegativeInfinity.ToString()).IsNone  |@ "Parse -Inf" .&.
        t.Parse(Double.PositiveInfinity.ToString()).IsNone  |@ "Parse +Inf"
        
    [<Property>]
    let ``Serialized numbers produce strings with exactly fraction-digits digits`` (FractionDigits digits) (FiniteFloat n) =
        isInDecimal64Range (FractionDigits digits) n ==> (
            let t = makeDecimal64Type (FractionDigits digits)
            match t.Serialize(n) with
            | None -> false
            | Some str -> str.Length - str.IndexOf('.') - 1 = digits
        )

    [<Property>]
    let ``Parses and serializes numbers only between the range Int64_MinValue * 10^-digits and Int64_MaxValue * 10^-digits`` (FractionDigits digits) (FiniteFloat n) =
        let t = makeDecimal64Type (FractionDigits digits)
        let isInRange = isInDecimal64Range (FractionDigits digits) n
        let nString = n.ToString("0.0#################", NumberFormatInfo.InvariantInfo)

        (isInRange = t.Serialize(n).IsSome)     |@ "Serialize" .&.
        (isInRange = t.Parse(nString).IsSome)   |@ "Parse"

    [<Property>]
    let ``Parsing and serializing can be combined`` (digits: FractionDigits) (FiniteFloat n) =
        let t = makeDecimal64Type digits
        (isInDecimal64Range digits n) ==> lazy (
            let serialized = t.Serialize(n).Value
            // We can't use `n` because the serialization might have removed some digits
            t.Parse(serialized).Value :?> float = Double.Parse(serialized, NumberFormatInfo.InvariantInfo)
        )