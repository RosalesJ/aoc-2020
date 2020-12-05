open Core
open Stdio
open Angstrom

type 'a or_invalid = Valid of 'a | Invalid of String.t

type field =
  | BirthYear of int
  | IssueYear of int
  | ExpirationYear of int
  | Height of [`In | `Cm] * int
  | HairColor of String.t
  | EyeColor of String.t
  | PassportId of string
  | CountryId of int

let field_name = function
  | BirthYear _ -> "byr"
  | IssueYear _ -> "iyr"
  | ExpirationYear _ -> "eyr"
  | Height _ -> "hgt"
  | HairColor _ -> "hcl"
  | EyeColor _ -> "ecl"
  | PassportId _ -> "pid"
  | CountryId _ -> "cid"

let show = function
  | BirthYear x -> sprintf "byr:%d" x
  | IssueYear x -> sprintf "iyr:%d" x
  | ExpirationYear x -> sprintf "eyr:%d" x
  | Height (`Cm, ht) -> sprintf "hgt:%dcm" ht
  | Height (`In, ht) -> sprintf "hgt:%din" ht
  | HairColor x -> sprintf "hcl:%s" x
  | EyeColor x -> sprintf "ecl:%s" x
  | PassportId x -> sprintf "pid:%s" x
  | CountryId x -> sprintf "cid:%d" x

let valid_eye_colors = ["amb"; "blu" ; "brn"; "gry"; "grn"; "hzl"; "oth"]

let valid = function
| BirthYear yr -> 1920 <= yr && yr <= 2002
| IssueYear yr -> 2010 <= yr && yr <= 2020
| ExpirationYear yr -> 2020 <= yr && yr <= 2030
| Height (`Cm, ht) -> 150 <= ht && ht <= 193
| Height (`In, ht) -> 59 <= ht && ht <= 76
| HairColor col -> Char.equal '#' (String.get col 0) && String.length col = 7
| EyeColor col -> List.mem valid_eye_colors ~equal:String.equal col
| PassportId id -> String.length id = 9
| CountryId _ -> true

let digits = take_while1 Char.is_digit
let num = digits >>| Int.of_string
let anything = take_while1 (function '\n' | ' ' -> false | _ -> true)
let unit = string "in" *> return `In
       <|> string "cm" *> return `Cm

let delimited p =
  p >>= fun result ->
  peek_char >>= function
  | Some ' ' | Some '\n' | None -> return result
  | Some _ -> fail "Parsing of token did not end"

let make_field tag p = string tag *> char ':' *> delimited p

let birth_year = make_field "byr" (num >>| fun x -> BirthYear x)
let issue_year = make_field "iyr" (num >>| fun x -> IssueYear x)
let expiration_year = make_field "eyr" (num >>| fun x -> ExpirationYear x)
let height = make_field "hgt" ((fun ht un -> Height (un, ht)) <$> num <*> unit)
let hair_color = make_field "hcl" (take_while1 (function '#' | '0'..'9' | 'a'..'f' -> true | _ -> false) >>| fun x -> HairColor x)
let eye_color = make_field "ecl" (anything >>| fun x -> EyeColor x)
let passport_id = make_field "pid" (digits >>| fun x -> PassportId x)
let country_id = make_field "cid" (num >>| fun x -> CountryId x)

let field = birth_year
        <|> issue_year
        <|> expiration_year
        <|> height
        <|> hair_color
        <|> eye_color
        <|> passport_id
        <|> country_id

let passport =
  let field_or_invalid = (field >>| fun x -> Valid x) <|> (anything >>| fun x -> Invalid x) in
  sep_by (char ' ' <|> char '\n') field_or_invalid

let validate_passport xs =
  List.exists xs ~f:(function Valid (BirthYear      _ as x) -> valid x | _ -> false) &&
  List.exists xs ~f:(function Valid (IssueYear      _ as x) -> valid x | _ -> false) &&
  List.exists xs ~f:(function Valid (ExpirationYear _ as x) -> valid x | _ -> false) &&
  List.exists xs ~f:(function Valid (Height         _ as x) -> valid x | _ -> false) &&
  List.exists xs ~f:(function Valid (HairColor      _ as x) -> valid x | _ -> false) &&
  List.exists xs ~f:(function Valid (EyeColor       _ as x) -> valid x | _ -> false) &&
  List.exists xs ~f:(function Valid (PassportId     _ as x) -> valid x | _ -> false) &&
  List.exists xs ~f:(function Valid (BirthYear      _ as x) -> valid x | _ -> false)

let passport_processing input =
  match Angstrom.parse_string (sep_by (string "\n\n") passport) input ~consume:Prefix with
  | Error _ -> 0
  | Ok xs ->
    let results = List.map xs ~f:validate_passport in
    List.count results ~f:Fn.id

let () =
  In_channel.create "./src/aoc4/input.txt"
  |> In_channel.input_all
  |> passport_processing
  |> Stdio.printf "\n%d\n"