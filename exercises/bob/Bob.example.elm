module Bob exposing (hey)


hey : String -> String
hey remark =
    let
        trimmedRemark =
            String.trim remark
    in
    if isShouting trimmedRemark && isQuestion trimmedRemark then
        "Calm down, I know what I'm doing!"

    else if isShouting trimmedRemark then
        "Whoa, chill out!"

    else if isQuestion trimmedRemark then
        "Sure."

    else if isSilence trimmedRemark then
        "Fine. Be that way!"

    else
        "Whatever."


isShouting : String -> Bool
isShouting remark =
    isUppercase remark && hasCharacters remark


isUppercase : String -> Bool
isUppercase remark =
    remark == String.toUpper remark


isQuestion : String -> Bool
isQuestion remark =
    String.endsWith "?" remark


hasCharacters : String -> Bool
hasCharacters remark =
    String.any Char.isAlpha remark


isSilence : String -> Bool
isSilence =
    String.isEmpty
