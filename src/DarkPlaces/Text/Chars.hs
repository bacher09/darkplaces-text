module DarkPlaces.Text.Chars (
    decodeQFontASCII,
    decodeQFontUTF
) where
import Data.Vector
import Data.Char
import DarkPlaces.Text.Classes


-- from https://github.com/xonotic/darkplaces/blob/master/console.c#L116
qfontAsciiTable :: Vector Char
qfontAsciiTable = fromList [
     '\0', '#',  '#',  '#',  '#',  '.',  '#',  '#',
     '#',  '\t', '\n', '#',  ' ',  '\r', '.',  '.',
     '[',  ']',  '0',  '1',  '2',  '3',  '4',  '5',
     '6',  '7',  '8',  '9',  '.',  '<',  '=',  '>',
     ' ',  '!',  '"',  '#',  '$',  '%',  '&',  '\'',
     '(',  ')',  '*',  '+',  ',',  '-',  '.',  '/',
     '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
     '8',  '9',  ':',  ';',  '<',  '=',  '>',  '?',
     '@',  'A',  'B',  'C',  'D',  'E',  'F',  'G',
     'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
     'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
     'X',  'Y',  'Z',  '[',  '\\', ']',  '^',  '_',
     '`',  'a',  'b',  'c',  'd',  'e',  'f',  'g',
     'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',
     'p',  'q',  'r',  's',  't',  'u',  'v',  'w',
     'x',  'y',  'z',  '{',  '|',  '}',  '~',  '<',
     '<',  '=',  '>',  '#',  '#',  '.',  '#',  '#',
     '#',  '#',  ' ',  '#',  ' ',  '>',  '.',  '.',
     '[',  ']',  '0',  '1',  '2',  '3',  '4',  '5',
     '6',  '7',  '8',  '9',  '.',  '<',  '=',  '>',
     ' ',  '!',  '"',  '#',  '$',  '%',  '&',  '\'',
     '(',  ')',  '*',  '+',  ',',  '-',  '.',  '/',
     '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
     '8',  '9',  ':',  ';',  '<',  '=',  '>',  '?',
     '@',  'A',  'B',  'C',  'D',  'E',  'F',  'G',
     'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
     'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
     'X',  'Y',  'Z',  '[',  '\\', ']',  '^',  '_',
     '`',  'a',  'b',  'c',  'd',  'e',  'f',  'g',
     'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',
     'p',  'q',  'r',  's',  't',  'u',  'v',  'w',
     'x',  'y',  'z',  '{',  '|',  '}',  '~',  '<'
    ]


-- from https://github.com/antzucaro/XonStat/blob/master/xonstat/util.py#L64
qfontUnicodeTable :: Vector Char
qfontUnicodeTable = fromList [
    '\32',      '\32',      '\8212',    '\32',
    '\95',      '\10055',   '\8224',    '\183',
    '\128299',  '\32',      '\32',      '\9632',
    '\8226',    '\8594',    '\10056',   '\10056',
    '\91',      '\93',      '\128125',  '\128527',
    '\128542',  '\128565',  '\128533',  '\128522',
    '\171',     '\187',     '\8226',    '\8254',
    '\10056',   '\9644',    '\9644',    '\9644',
    '\32',      '\33',      '\34',      '\35',
    '\36',      '\37',      '\38',      '\39',
    '\40',      '\41',      '\215',     '\43',
    '\44',      '\45',      '\46',      '\47',
    '\48',      '\49',      '\50',      '\51',
    '\52',      '\53',      '\54',      '\55',
    '\56',      '\57',      '\58',      '\59',
    '\60',      '\61',      '\62',      '\63',
    '\64',      '\65',      '\66',      '\67',
    '\68',      '\69',      '\70',      '\71',
    '\72',      '\73',      '\74',      '\75',
    '\76',      '\77',      '\78',      '\79',
    '\80',      '\81',      '\82',      '\83',
    '\84',      '\85',      '\86',      '\87',
    '\88',      '\89',      '\90',      '\91',
    '\92',      '\93',      '\94',      '\95',
    '\39',      '\97',      '\98',      '\99',
    '\100',     '\101',     '\102',     '\103',
    '\104',     '\105',     '\106',     '\107',
    '\108',     '\109',     '\110',     '\111',
    '\112',     '\113',     '\114',     '\115',
    '\116',     '\117',     '\118',     '\119',
    '\120',     '\121',     '\122',     '\123',
    '\124',     '\125',     '\126',     '\8592',
    '\60',      '\61',      '\62',      '\128640',
    '\161',     '\79',      '\85',      '\73',
    '\67',      '\169',     '\174',     '\9632',
    '\191',     '\9654',    '\10056',   '\10056',
    '\10098',   '\10099',   '\128125',  '\128527',
    '\128542',  '\128565',  '\128533',  '\128522',
    '\171',     '\187',     '\10055',   '\120',
    '\10056',   '\8212',    '\8212',    '\8212',
    '\32',      '\33',      '\34',      '\35',
    '\36',      '\37',      '\38',      '\39',
    '\40',      '\41',      '\42',      '\43',
    '\44',      '\45',      '\46',      '\47',
    '\48',      '\49',      '\50',      '\51',
    '\52',      '\53',      '\54',      '\55',
    '\56',      '\57',      '\58',      '\59',
    '\60',      '\61',      '\62',      '\63',
    '\64',      '\65',      '\66',      '\67',
    '\68',      '\69',      '\70',      '\71',
    '\72',      '\73',      '\74',      '\75',
    '\76',      '\77',      '\78',      '\79',
    '\80',      '\81',      '\82',      '\83',
    '\84',      '\85',      '\86',      '\87',
    '\88',      '\89',      '\90',      '\91',
    '\92',      '\93',      '\94',      '\95',
    '\39',      '\65',      '\66',      '\67',
    '\68',      '\69',      '\70',      '\71',
    '\72',      '\73',      '\74',      '\75',
    '\76',      '\77',      '\78',      '\79',
    '\80',      '\81',      '\82',      '\83',
    '\84',      '\85',      '\86',      '\87',
    '\88',      '\89',      '\90',      '\123',
    '\124',     '\125',     '\126',     '\9664'
    ]


decodeQFont :: (CharMap a) => Vector Char -> a -> a
decodeQFont qtable = mapChars replace_char
  where
    replace_char c = if '\xe000' <= c && c <= '\xe0ff'
        then qtable ! (ord c - 0xe000)
        else c


decodeQFontOld :: (CharMap a) => Vector Char -> a -> a
decodeQFontOld qtable = mapChars replace_char
  where
    replace_char c = if '\0' <= c && c <= '\255' && c /= '\n'
        then qtable ! ord c
        else c


decodeQFontASCII :: (CharMap a) => Bool -> a -> a
decodeQFontASCII True = decodeQFont qfontAsciiTable
decodeQFontASCII False = decodeQFontOld qfontAsciiTable


decodeQFontUTF :: (CharMap a) => Bool -> a -> a
decodeQFontUTF True = decodeQFont qfontUnicodeTable
decodeQFontUTF False = decodeQFontOld qfontUnicodeTable
