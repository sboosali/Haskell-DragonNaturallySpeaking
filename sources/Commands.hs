module Commands where
import Commands.Types


touch :: Char -> KeyPress

touch 'a'  = Press [      ] AKey
touch 'A'  = Press [Shift ] AKey
touch 'b'  = Press [      ] BKey
touch 'B'  = Press [Shift ] BKey
touch 'c'  = Press [      ] CKey
touch 'C'  = Press [Shift ] CKey
touch 'd'  = Press [      ] DKey
touch 'D'  = Press [Shift ] DKey
touch 'e'  = Press [      ] EKey
touch 'E'  = Press [Shift ] EKey
touch 'f'  = Press [      ] FKey
touch 'F'  = Press [Shift ] FKey
touch 'g'  = Press [      ] GKey
touch 'G'  = Press [Shift ] GKey
touch 'h'  = Press [      ] HKey
touch 'H'  = Press [Shift ] HKey
touch 'i'  = Press [      ] IKey
touch 'I'  = Press [Shift ] IKey
touch 'j'  = Press [      ] JKey
touch 'J'  = Press [Shift ] JKey
touch 'k'  = Press [      ] KKey
touch 'K'  = Press [Shift ] KKey
touch 'l'  = Press [      ] LKey
touch 'L'  = Press [Shift ] LKey
touch 'm'  = Press [      ] MKey
touch 'M'  = Press [Shift ] MKey
touch 'n'  = Press [      ] NKey
touch 'N'  = Press [Shift ] NKey
touch 'o'  = Press [      ] OKey
touch 'O'  = Press [Shift ] OKey
touch 'p'  = Press [      ] PKey
touch 'P'  = Press [Shift ] PKey
touch 'q'  = Press [      ] QKey
touch 'Q'  = Press [Shift ] QKey
touch 'r'  = Press [      ] RKey
touch 'R'  = Press [Shift ] RKey
touch 's'  = Press [      ] SKey
touch 'S'  = Press [Shift ] SKey
touch 't'  = Press [      ] TKey
touch 'T'  = Press [Shift ] TKey
touch 'u'  = Press [      ] UKey
touch 'U'  = Press [Shift ] UKey
touch 'v'  = Press [      ] VKey
touch 'V'  = Press [Shift ] VKey
touch 'w'  = Press [      ] WKey
touch 'W'  = Press [Shift ] WKey
touch 'x'  = Press [      ] XKey
touch 'X'  = Press [Shift ] XKey
touch 'y'  = Press [      ] YKey
touch 'Y'  = Press [Shift ] YKey
touch 'z'  = Press [      ] ZKey
touch 'Z'  = Press [Shift ] ZKey

touch '0'  = Press [      ] ZeroKey
touch ')'  = Press [Shift ] ZeroKey
touch '1'  = Press [      ] OneKey
touch '!'  = Press [Shift ] OneKey
touch '2'  = Press [      ] TwoKey
touch '@'  = Press [Shift ] TwoKey
touch '3'  = Press [      ] ThreeKey
touch '#'  = Press [Shift ] ThreeKey
touch '4'  = Press [      ] FourKey
touch '$'  = Press [Shift ] FourKey
touch '5'  = Press [      ] FiveKey
touch '%'  = Press [Shift ] FiveKey
touch '6'  = Press [      ] SixKey
touch '^'  = Press [Shift ] SixKey
touch '7'  = Press [      ] SevenKey
touch '&'  = Press [Shift ] SevenKey
touch '8'  = Press [      ] EightKey
touch '*'  = Press [Shift ] EightKey
touch '9'  = Press [      ] NineKey
touch '('  = Press [Shift ] NineKey

touch '`'  = Press [      ] GraveKey
touch '~'  = Press [Shift ] GraveKey
touch '-'  = Press [      ] MinusKey
touch '_'  = Press [Shift ] MinusKey
touch '='  = Press [      ] EqualKey
touch '+'  = Press [Shift ] EqualKey
touch '['  = Press [      ] LeftBracketKey
touch '{'  = Press [Shift ] LeftBracketKey
touch ']'  = Press [      ] RightBracketKey
touch '}'  = Press [Shift ] RightBracketKey
touch '\\' = Press [      ] BackslashKey
touch '|'  = Press [Shift ] BackslashKey
touch ';'  = Press [      ] SemicolonKey
touch ':'  = Press [Shift ] SemicolonKey
touch '\'' = Press [      ] QuoteKey
touch '"'  = Press [Shift ] QuoteKey
touch ','  = Press [      ] CommaKey
touch '<'  = Press [Shift ] CommaKey
touch '.'  = Press [      ] PeriodKey
touch '>'  = Press [Shift ] PeriodKey
touch '/'  = Press [      ] SlashKey
touch '?'  = Press [Shift ] SlashKey

touch ' '  = Press [      ] SpaceKey
touch '\t' = Press [      ] TabKey
touch '\n' = Press [      ] ReturnKey

