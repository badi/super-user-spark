module Constants where

keywordCard :: String
keywordCard = "card"

keywordSpark :: String
keywordSpark = "spark"

keywordFile :: String
keywordFile = "file"

keywordInto :: String
keywordInto = "into"

keywordOutof :: String
keywordOutof = "outof"

keywordKindOverride :: String
keywordKindOverride = "kind"

keywordLink :: String
keywordLink = "link"

keywordCopy :: String
keywordCopy = "copy"

keywordAlternatives :: String
keywordAlternatives = "alternatives"

linkKindSymbol :: String
linkKindSymbol = "l->"

copyKindSymbol :: String
copyKindSymbol = "c->"

pipeKindSymbolOpen :: String
pipeKindSymbolOpen = "-["

pipeKindSymbolClose :: String
pipeKindSymbolClose = "]-"

unspecifiedKindSymbol :: String
unspecifiedKindSymbol = "->"

bracesChars :: [Char]
bracesChars = ['{','}']

linespaceChars :: [Char]
linespaceChars = [' ', '\t']

endOfLineChars :: [Char]
endOfLineChars = ['\n','\r']

whitespaceChars :: [Char]
whitespaceChars = linespaceChars ++ endOfLineChars

lineDelimiter :: String
lineDelimiter = ";"

branchDelimiter :: String
branchDelimiter = ":"

quotesChar :: Char
quotesChar = '"'

lineCommentStr :: String
lineCommentStr = "#"

blockCommentStrs :: (String, String)
blockCommentStrs = ("[[", "]]")

sparkExtension :: String
sparkExtension = "sus"
