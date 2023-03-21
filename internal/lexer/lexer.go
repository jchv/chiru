package lexer

import (
	"log"
	"unicode/utf8"

	"github.com/smasher164/xid"
)

//go:generate go run golang.org/x/tools/cmd/stringer -type=Token

type Token int

const (
	TEndOfSource Token = iota + 1
	TInvalid

	// Constants
	TIntegerConstant
	TFloatConstant
	TStringLiteral
	TCharLiteral

	// Operators
	TEllipsis
	TRightShiftEqual
	TLeftShiftEqual
	TPlusEqual
	TMinusEqual
	TAsteriskEqual
	TSlashEqual
	TPercentEqual
	TAndEqual
	TXorEqual
	TOrEqual
	TRightShift
	TLeftShift
	TPlusPlus
	TMinusMinus
	TPointerDot
	TAndAnd
	TOrOr
	TLessEqual
	TGreaterEqual
	TEqualEqual
	TNotEqual
	TSemicolon
	TCurlyBraceOpen
	TCurlyBraceClose
	TComma
	TColon
	TEqual
	TParenOpen
	TParenClose
	TSquareBracketOpen
	TSquareBracketClose
	TDot
	TAnd
	TNot
	TBitNot
	TMinus
	TPlus
	TAsterisk
	TSlash
	TPercent
	TLess
	TGreater
	TXor
	TOr
	TQuestion

	// Identifier/Keywords
	TIdentifier
	TAlignAs
	TAlignOf
	TAtomic
	TAuto
	TBitInt
	TBool
	TBreak
	TCase
	TChar
	TComplex
	TConst
	TConstExpr
	TContinue
	TDecimal128
	TDecimal32
	TDecimal64
	TDefault
	TDo
	TDouble
	TElse
	TEnum
	TExtern
	TFalse
	TFloat
	TFor
	TGeneric
	TGoto
	TIf
	TImaginary
	TInline
	TInt
	TLong
	TNoreturn
	TNullptr
	TRegister
	TRestrict
	TReturn
	TShort
	TSigned
	TSizeof
	TStatic
	TStaticAssert
	TStruct
	TSwitch
	TThreadLocal
	TTrue
	TTypedef
	TTypeof
	TTypeofUnqual
	TUnion
	TUnsigned
	TVoid
	TVolatile
	TWhile
)

var Keywords = map[string]Token{
	"_Alignas":       TAlignAs,
	"_Alignof":       TAlignOf,
	"_Atomic":        TAtomic,
	"_BitInt":        TBitInt,
	"_Bool":          TBool,
	"_Complex":       TComplex,
	"_Decimal128":    TDecimal128,
	"_Decimal32":     TDecimal32,
	"_Decimal64":     TDecimal64,
	"_Generic":       TGeneric,
	"_Imaginary":     TImaginary,
	"_Noreturn":      TNoreturn,
	"_Static_assert": TStaticAssert,
	"_Thread_local":  TThreadLocal,
	"alignas":        TAlignAs,
	"alignof":        TAlignOf,
	"auto":           TAuto,
	"bool":           TBool,
	"break":          TBreak,
	"case":           TCase,
	"char":           TChar,
	"const":          TConst,
	"constexpr":      TConstExpr,
	"continue":       TContinue,
	"default":        TDefault,
	"do":             TDo,
	"double":         TDouble,
	"else":           TElse,
	"enum":           TEnum,
	"extern":         TExtern,
	"false":          TFalse,
	"float":          TFloat,
	"for":            TFor,
	"goto":           TGoto,
	"if":             TIf,
	"inline":         TInline,
	"int":            TInt,
	"long":           TLong,
	"nullptr":        TNullptr,
	"register":       TRegister,
	"restrict":       TRestrict,
	"return":         TReturn,
	"short":          TShort,
	"signed":         TSigned,
	"sizeof":         TSizeof,
	"static_assert":  TStaticAssert,
	"static":         TStatic,
	"struct":         TStruct,
	"switch":         TSwitch,
	"thread_local":   TThreadLocal,
	"true":           TTrue,
	"typedef":        TTypedef,
	"typeof_unqual":  TTypeofUnqual,
	"typeof":         TTypeof,
	"union":          TUnion,
	"unsigned":       TUnsigned,
	"void":           TVoid,
	"volatile":       TVolatile,
	"while":          TWhile,
}

type Lexer struct {
	codePoint rune
	current   int
	start     int
	end       int
	source    string
	Token     Token
	Raw       string
}

func NewLexer(source string) Lexer {
	lexer := Lexer{source: source}
	lexer.step()
	lexer.Next()
	return lexer
}

func (lexer *Lexer) step() {
	codePoint, width := utf8.DecodeRuneInString(lexer.source[lexer.current:])
	if width == 0 {
		codePoint = -1
	}
	lexer.codePoint = codePoint
	lexer.end = lexer.current
	lexer.current += width
}

func (lexer *Lexer) raw() string {
	return lexer.source[lexer.start:lexer.end]
}

func (lexer *Lexer) assumePreprocessorDirective() {
	lexer.step()
	for lexer.codePoint == ' ' || lexer.codePoint == '\t' {
		lexer.step()
	}
	switch lexer.codePoint {
	case 'd':
		if lexer.step(); lexer.codePoint != 'e' {
			lexer.invalidPreprocessorDirective()
		}
		if lexer.step(); lexer.codePoint != 'f' {
			lexer.invalidPreprocessorDirective()
		}
		if lexer.step(); lexer.codePoint != 'i' {
			lexer.invalidPreprocessorDirective()
		}
		if lexer.step(); lexer.codePoint != 'n' {
			lexer.invalidPreprocessorDirective()
		}
		if lexer.step(); lexer.codePoint != 'e' {
			lexer.invalidPreprocessorDirective()
		}
		lexer.step()
		lexer.assumePreprocessorDirectiveDefine()
	case 'e':
		lexer.step()
		switch lexer.codePoint {
		case 'n':
			if lexer.step(); lexer.codePoint != 'd' {
				lexer.invalidPreprocessorDirective()
			}
			if lexer.step(); lexer.codePoint != 'i' {
				lexer.invalidPreprocessorDirective()
			}
			if lexer.step(); lexer.codePoint != 'f' {
				lexer.invalidPreprocessorDirective()
			}
			lexer.step()
			lexer.assumePreprocessorDirectiveEndif()
		case 'm':
			if lexer.step(); lexer.codePoint != 'b' {
				lexer.invalidPreprocessorDirective()
			}
			if lexer.step(); lexer.codePoint != 'e' {
				lexer.invalidPreprocessorDirective()
			}
			if lexer.step(); lexer.codePoint != 'd' {
				lexer.invalidPreprocessorDirective()
			}
			lexer.step()
			lexer.assumePreprocessorDirectiveEmbed()
		case 'l':
			lexer.step()
			switch lexer.codePoint {
			case 's':
				if lexer.step(); lexer.codePoint != 'e' {
					lexer.invalidPreprocessorDirective()
				}
				lexer.step()
				lexer.assumePreprocessorDirectiveElse()
			case 'i':
				if lexer.step(); lexer.codePoint != 'f' {
					lexer.invalidPreprocessorDirective()
				}
				lexer.step()
				switch lexer.codePoint {
				case 'd':
					if lexer.step(); lexer.codePoint != 'e' {
						lexer.invalidPreprocessorDirective()
					}
					if lexer.step(); lexer.codePoint != 'f' {
						lexer.invalidPreprocessorDirective()
					}
					lexer.step()
					lexer.assumePreprocessorDirectiveElifdef()
				case 'n':
					if lexer.step(); lexer.codePoint != 'd' {
						lexer.invalidPreprocessorDirective()
					}
					if lexer.step(); lexer.codePoint != 'e' {
						lexer.invalidPreprocessorDirective()
					}
					if lexer.step(); lexer.codePoint != 'f' {
						lexer.invalidPreprocessorDirective()
					}
					lexer.step()
					lexer.assumePreprocessorDirectiveElifndef()
				default:
					lexer.assumePreprocessorDirectiveElif()
				}
			}
		case 'r':
			if lexer.step(); lexer.codePoint != 'r' {
				lexer.invalidPreprocessorDirective()
			}
			if lexer.step(); lexer.codePoint != 'o' {
				lexer.invalidPreprocessorDirective()
			}
			if lexer.step(); lexer.codePoint != 'r' {
				lexer.invalidPreprocessorDirective()
			}
			lexer.step()
			lexer.assumePreprocessorDirectiveError()
		default:
			lexer.invalidPreprocessorDirective()
		}
	case 'i':
		lexer.step()
		switch lexer.codePoint {
		case 'n':
			if lexer.step(); lexer.codePoint != 'c' {
				lexer.invalidPreprocessorDirective()
			}
			if lexer.step(); lexer.codePoint != 'l' {
				lexer.invalidPreprocessorDirective()
			}
			if lexer.step(); lexer.codePoint != 'u' {
				lexer.invalidPreprocessorDirective()
			}
			if lexer.step(); lexer.codePoint != 'd' {
				lexer.invalidPreprocessorDirective()
			}
			if lexer.step(); lexer.codePoint != 'e' {
				lexer.invalidPreprocessorDirective()
			}
			lexer.step()
			lexer.assumePreprocessorDirectiveInclude()
		case 'f':
			lexer.step()
			switch lexer.codePoint {
			case 'n':
				if lexer.step(); lexer.codePoint != 'd' {
					lexer.invalidPreprocessorDirective()
				}
				if lexer.step(); lexer.codePoint != 'e' {
					lexer.invalidPreprocessorDirective()
				}
				if lexer.step(); lexer.codePoint != 'f' {
					lexer.invalidPreprocessorDirective()
				}
				lexer.step()
				lexer.assumePreprocessorDirectiveIfndef()
			case 'd':
				if lexer.step(); lexer.codePoint != 'e' {
					lexer.invalidPreprocessorDirective()
				}
				if lexer.step(); lexer.codePoint != 'f' {
					lexer.invalidPreprocessorDirective()
				}
				lexer.assumePreprocessorDirectiveIfdef()
			default:
				lexer.assumePreprocessorDirectiveIf()
			}
		default:
			lexer.invalidPreprocessorDirective()
		}
	case 'l':
		if lexer.step(); lexer.codePoint != 'i' {
			lexer.invalidPreprocessorDirective()
		}
		if lexer.step(); lexer.codePoint != 'n' {
			lexer.invalidPreprocessorDirective()
		}
		if lexer.step(); lexer.codePoint != 'e' {
			lexer.invalidPreprocessorDirective()
		}
		lexer.step()
		lexer.assumePreprocessorDirectiveLine()
	case 'p':
		if lexer.step(); lexer.codePoint != 'r' {
			lexer.invalidPreprocessorDirective()
		}
		if lexer.step(); lexer.codePoint != 'a' {
			lexer.invalidPreprocessorDirective()
		}
		if lexer.step(); lexer.codePoint != 'g' {
			lexer.invalidPreprocessorDirective()
		}
		if lexer.step(); lexer.codePoint != 'm' {
			lexer.invalidPreprocessorDirective()
		}
		if lexer.step(); lexer.codePoint != 'a' {
			lexer.invalidPreprocessorDirective()
		}
		lexer.step()
		lexer.assumePreprocessorDirectivePragma()
	case 'u':
		if lexer.step(); lexer.codePoint != 'n' {
			lexer.invalidPreprocessorDirective()
		}
		if lexer.step(); lexer.codePoint != 'd' {
			lexer.invalidPreprocessorDirective()
		}
		if lexer.step(); lexer.codePoint != 'e' {
			lexer.invalidPreprocessorDirective()
		}
		if lexer.step(); lexer.codePoint != 'f' {
			lexer.invalidPreprocessorDirective()
		}
		lexer.step()
		lexer.assumePreprocessorDirectiveUndef()
	case 'w':
		if lexer.step(); lexer.codePoint != 'a' {
			lexer.invalidPreprocessorDirective()
		}
		if lexer.step(); lexer.codePoint != 'r' {
			lexer.invalidPreprocessorDirective()
		}
		if lexer.step(); lexer.codePoint != 'n' {
			lexer.invalidPreprocessorDirective()
		}
		if lexer.step(); lexer.codePoint != 'i' {
			lexer.invalidPreprocessorDirective()
		}
		if lexer.step(); lexer.codePoint != 'n' {
			lexer.invalidPreprocessorDirective()
		}
		if lexer.step(); lexer.codePoint != 'g' {
			lexer.invalidPreprocessorDirective()
		}
		lexer.step()
		lexer.assumePreprocessorDirectiveWarning()
	default:
		lexer.invalidPreprocessorDirective()
	}
}

func (lexer *Lexer) assumePreprocessorDirectiveDefine()   { lexer.unimplementedDirective("#define") }
func (lexer *Lexer) assumePreprocessorDirectiveElif()     { lexer.unimplementedDirective("#elif") }
func (lexer *Lexer) assumePreprocessorDirectiveElifdef()  { lexer.unimplementedDirective("#elifdef") }
func (lexer *Lexer) assumePreprocessorDirectiveElifndef() { lexer.unimplementedDirective("#elifndef") }
func (lexer *Lexer) assumePreprocessorDirectiveElse()     { lexer.unimplementedDirective("#else") }
func (lexer *Lexer) assumePreprocessorDirectiveEmbed()    { lexer.unimplementedDirective("#embed") }
func (lexer *Lexer) assumePreprocessorDirectiveEndif()    { lexer.unimplementedDirective("#endif") }
func (lexer *Lexer) assumePreprocessorDirectiveError()    { lexer.unimplementedDirective("#error") }
func (lexer *Lexer) assumePreprocessorDirectiveIf()       { lexer.unimplementedDirective("#if") }
func (lexer *Lexer) assumePreprocessorDirectiveIfdef()    { lexer.unimplementedDirective("#ifdef") }
func (lexer *Lexer) assumePreprocessorDirectiveIfndef()   { lexer.unimplementedDirective("#ifndef") }
func (lexer *Lexer) assumePreprocessorDirectiveInclude()  { lexer.unimplementedDirective("#include") }
func (lexer *Lexer) assumePreprocessorDirectiveLine()     { lexer.unimplementedDirective("#line") }
func (lexer *Lexer) assumePreprocessorDirectivePragma()   { lexer.unimplementedDirective("#pragma") }
func (lexer *Lexer) assumePreprocessorDirectiveUndef()    { lexer.unimplementedDirective("#undef") }
func (lexer *Lexer) assumePreprocessorDirectiveWarning()  { lexer.unimplementedDirective("#warning") }

func (lexer *Lexer) unimplementedDirective(directive string) {
	log.Printf("Warning: %s not implemented\n", directive)
	for {
		switch lexer.codePoint {
		case '\\':
			lexer.step()
			if lexer.codePoint == '\r' {
				lexer.step()
				if lexer.codePoint == '\n' {
					lexer.step()
				}
			} else if lexer.codePoint == '\n' {
				lexer.step()
			}
		case -1, '\r', '\n', '\u2028', '\u2029':
			return
		}
		lexer.step()
	}
}

func (lexer *Lexer) invalidPreprocessorDirective() {
	panic("invalid preprocessor directive: " + lexer.raw())
}

func (lexer *Lexer) requireDecimalDigitSequence() {
	switch lexer.codePoint {
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		lexer.assumeDecimalDigitSequence()
	default:
		panic("expected decimal digit sequence")
	}
}

func (lexer *Lexer) assumeDecimalDigitSequence() {
	var n, i int
	for n, i = len(lexer.source), lexer.current; i < n; i++ {
		c := lexer.source[i]
		if c == '\'' {
			i++
			c = lexer.source[i]
			if c < '0' || c > '9' {
				panic("invalid digit sequence")
			}
			continue
		}
		if c < '0' || c > '9' {
			break
		}
	}
	lexer.current = i
	lexer.step()
}

func (lexer *Lexer) requireHexadecimalDigitSequence() {
	switch lexer.codePoint {
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
		'a', 'b', 'c', 'd', 'e', 'f',
		'A', 'B', 'C', 'D', 'E', 'F':
		lexer.assumeHexadecimalDigitSequence()
	default:
		panic("expected decimal digit sequence")
	}
}

func (lexer *Lexer) maybeHexadecimalDigitSequence() {
	switch lexer.codePoint {
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
		'a', 'b', 'c', 'd', 'e', 'f',
		'A', 'B', 'C', 'D', 'E', 'F':
		lexer.assumeHexadecimalDigitSequence()
	default:
	}
}

func (lexer *Lexer) assumeHexadecimalDigitSequence() {
	var n, i int
	for n, i = len(lexer.source), lexer.current; i < n; i++ {
		c := lexer.source[i]
		if c == '\'' {
			i++
			c = lexer.source[i]
			if (c < '0' || c > '9') && (c < 'a' || c > 'f') && (c < 'A' || c > 'F') {
				panic("invalid hex digit sequence")
			}
			continue
		}
		if (c < '0' || c > '9') && (c < 'a' || c > 'f') && (c < 'A' || c > 'F') {
			break
		}
	}
	lexer.current = i
	lexer.step()
}

func (lexer *Lexer) assumeOctalDigitSequence() {
	var n, i int
	for n, i = len(lexer.source), lexer.current; i < n; i++ {
		c := lexer.source[i]
		if c == '\'' {
			i++
			c = lexer.source[i]
			if c < '0' || c > '7' {
				panic("invalid digit sequence")
			}
			continue
		}
		if c < '0' || c > '7' {
			break
		}
	}
	lexer.current = i
	lexer.step()
}

func (lexer *Lexer) requireBinaryDigitSequence() {
	switch lexer.codePoint {
	case '0', '1':
		lexer.assumeBinaryDigitSequence()
	default:
		panic("expected binary digit sequence")
	}
}

func (lexer *Lexer) assumeBinaryDigitSequence() {
	var n, i int
	for n, i = len(lexer.source), lexer.current; i < n; i++ {
		c := lexer.source[i]
		if c == '\'' {
			i++
			c = lexer.source[i]
			if c != '0' && c != '1' {
				panic("invalid digit sequence")
			}
			continue
		}
		if c != '0' && c != '1' {
			break
		}
	}
	lexer.current = i
	lexer.step()
}

func (lexer *Lexer) maybeIntegerSuffix() {
	switch lexer.codePoint {
	case 'u', 'U':
		lexer.step()
		switch lexer.codePoint {
		case 'l':
			lexer.step()
			if lexer.codePoint == 'l' {
				lexer.step()
			}
		case 'L':
			lexer.step()
			if lexer.codePoint == 'L' {
				lexer.step()
			}
		case 'w':
			if lexer.codePoint == 'b' {
				lexer.step()
			} else {
				panic("invalid integer suffix")
			}
		case 'W':
			if lexer.codePoint == 'B' {
				lexer.step()
			} else {
				panic("invalid integer suffix")
			}
		}

	case 'l':
		lexer.step()
		if lexer.codePoint == 'l' {
			lexer.step()
		}
		if lexer.codePoint == 'u' || lexer.codePoint == 'U' {
			lexer.step()
		}

	case 'L':
		lexer.step()
		if lexer.codePoint == 'L' {
			lexer.step()
		}
		if lexer.codePoint == 'u' || lexer.codePoint == 'U' {
			lexer.step()
		}

	case 'w':
		lexer.step()
		if lexer.codePoint == 'b' {
			lexer.step()
		} else {
			panic("invalid integer suffix")
		}
		if lexer.codePoint == 'u' || lexer.codePoint == 'U' {
			lexer.step()
		}

	case 'W':
		lexer.step()
		if lexer.codePoint == 'B' {
			lexer.step()
		} else {
			panic("invalid integer suffix")
		}
		if lexer.codePoint == 'u' || lexer.codePoint == 'U' {
			lexer.step()
		}
	}
}

func (lexer *Lexer) maybeDecimalExponent() {
	if lexer.codePoint == 'e' || lexer.codePoint == 'E' {
		lexer.assumeDecimalExponent()
	}
}

func (lexer *Lexer) assumeDecimalExponent() {
	lexer.step()
	if lexer.codePoint == '-' || lexer.codePoint == '+' {
		lexer.step()
	}
	lexer.requireDecimalDigitSequence()
}

func (lexer *Lexer) requireBinaryExponent() {
	if lexer.codePoint != 'p' && lexer.codePoint != 'P' {
		panic("missing binary exponent")
	}
	lexer.step()
	if lexer.codePoint == '-' || lexer.codePoint == '+' {
		lexer.step()
	}
	lexer.requireDecimalDigitSequence()
}

func (lexer *Lexer) maybeFloatingSuffix() {
	switch lexer.codePoint {
	case 'f', 'F', 'l', 'L':
		lexer.step()

	case 'd':
		lexer.step()
		switch lexer.codePoint {
		case 'f', 'd', 'l':
			lexer.step()
		default:
			panic("invalid float suffix")
		}

	case 'D':
		lexer.step()
		switch lexer.codePoint {
		case 'F', 'D', 'L':
			lexer.step()
		default:
			panic("invalid float suffix")
		}
	}
}

func (lexer *Lexer) assumeCharConstant(encodingPrefix string) {
	if encodingPrefix != "" {
		switch encodingPrefix {
		case "u8", "u", "U", "L":
		default:
			panic("invalid string encoding prefix")
		}
	}
	for {
		lexer.step()
		switch lexer.codePoint {
		case -1:
			panic("unexpected EOF")
		case '\\':
			panic("escape sequence not implemented")
		case '\'':
			lexer.step()
			return
		}
	}
}

func (lexer *Lexer) assumeStringLiteral(encodingPrefix string) {
	if encodingPrefix != "" {
		switch encodingPrefix {
		case "u8", "u", "U", "L":
		default:
			panic("invalid string encoding prefix")
		}
	}
	for {
		lexer.step()
		switch lexer.codePoint {
		case -1:
			panic("unexpected EOF")
		case '\\':
			panic("escape sequence not implemented")
		case '"':
			lexer.step()
		default:
			continue
		}
		break
	}
	lexer.Raw = lexer.raw()
	lexer.Token = TStringLiteral
}

func (lexer *Lexer) Next() {
	for {
		lexer.start = lexer.end
		lexer.Token = 0
		lexer.Raw = ""

		switch lexer.codePoint {
		case -1:
			lexer.Token = TEndOfSource

		// Whitespace
		case ' ', '\t', '\n', '\v', '\f', '\r', '\u0085', '\u00a0', '\u2028', '\u2029':
			lexer.step()
			continue

		// Preprocessor directive
		case '#':
			lexer.assumePreprocessorDirective()

		// Comment, /, /=
		case '/':
			lexer.step()
			switch lexer.codePoint {
			case '/':
			LineComment:
				for {
					lexer.step()
					switch lexer.codePoint {
					case -1, '\r', '\n', '\u2028', '\u2029':
						break LineComment
					}
				}
				continue

			case '*':
				lexer.step()
			BlockComment:
				for {
					switch lexer.codePoint {
					case '*':
						lexer.step()
						if lexer.codePoint == '/' {
							lexer.step()
							break BlockComment
						}

					case '\r', '\n', '\u2028', '\u2029':
						lexer.step()

					case -1:
						panic("unexpected EOF")

					default:
						lexer.step()
					}
				}
				continue

			case '=':
				lexer.step()
				lexer.Token = TSlashEqual

			default:
				lexer.Token = TSlash
			}

		// Identifier (ASCII)
		case '_',
			'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
			'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
			'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
			'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z':
			// Identifier (ASCII fast-path)
			var n, i int
			for n, i = len(lexer.source), lexer.current; i < n; i++ {
				c := lexer.source[i]
				if (c < 'a' || c > 'z') && (c < 'A' || c > 'Z') && (c < '0' || c > '9') && c != '_' {
					break
				}
			}
			lexer.current = i

			// Slow path for remaining characters
			lexer.step()
			if lexer.codePoint >= 0x80 {
				for xid.Continue(lexer.codePoint) {
					lexer.step()
				}
			}

			raw := lexer.raw()
			switch lexer.codePoint {
			case '\'':
				lexer.assumeCharConstant(raw)
			case '"':
				lexer.assumeStringLiteral(raw)
			default:
				lexer.Raw = raw
				lexer.Token = Keywords[raw]
				if lexer.Token == 0 {
					lexer.Token = TIdentifier
				}
			}

		// Octal, hexadecimal, binary integers, or floating point fraction.
		case '0':
			lexer.step()
			switch lexer.codePoint {
			case '0', '1', '2', '3', '4', '5', '6', '7':
				lexer.assumeOctalDigitSequence()
				lexer.maybeIntegerSuffix()
				lexer.Token = TIntegerConstant
				lexer.Raw = lexer.raw()

			case 'x', 'X':
				lexer.step()
				lexer.requireHexadecimalDigitSequence()
				switch lexer.codePoint {
				case '.':
					lexer.step()
					lexer.maybeHexadecimalDigitSequence()
					lexer.requireBinaryExponent()
					lexer.maybeFloatingSuffix()
					lexer.Token = TFloatConstant
					lexer.Raw = lexer.raw()

				default:
					lexer.maybeIntegerSuffix()
					lexer.Token = TIntegerConstant
					lexer.Raw = lexer.raw()
				}

			case 'b', 'B':
				lexer.step()
				lexer.requireBinaryDigitSequence()
				lexer.maybeIntegerSuffix()
				lexer.Token = TIntegerConstant
				lexer.Raw = lexer.raw()

			default:
				// Just zero.
				lexer.Token = TIntegerConstant
				lexer.Raw = lexer.raw()
			}

		// Decimal integer or decimal floating point number.
		case '1', '2', '3', '4', '5', '6', '7', '8', '9':
			lexer.assumeDecimalDigitSequence()
			switch lexer.codePoint {
			case '.':
				lexer.step()
				lexer.requireDecimalDigitSequence()
				lexer.maybeDecimalExponent()
				lexer.maybeFloatingSuffix()
				lexer.Token = TFloatConstant
				lexer.Raw = lexer.raw()

			case 'e', 'E':
				lexer.assumeDecimalExponent()
				lexer.maybeFloatingSuffix()
				lexer.Token = TFloatConstant
				lexer.Raw = lexer.raw()

			case 'u', 'U', 'l', 'L', 'w', 'W':
				lexer.maybeIntegerSuffix()
				lexer.Token = TIntegerConstant
				lexer.Raw = lexer.raw()

			default:
				lexer.Token = TIntegerConstant
				lexer.Raw = lexer.raw()
			}

		// Dot, ellipsis or decimal floating point number.
		case '.':
			lexer.step()
			if lexer.codePoint < '0' || lexer.codePoint > '9' {
				if lexer.codePoint == '.' {
					lexer.step()
					if lexer.codePoint != '.' {
						panic("invalid token ..")
					}
					lexer.Token = TEllipsis
					break
				}
				lexer.Token = TDot
				break
			}
			lexer.assumeDecimalDigitSequence()
			lexer.maybeDecimalExponent()
			lexer.maybeFloatingSuffix()
			lexer.Token = TFloatConstant
			lexer.Raw = lexer.raw()

		case '\'':
			lexer.assumeCharConstant("")

		case '"':
			lexer.assumeStringLiteral("")

		case '>':
			lexer.step()
			switch lexer.codePoint {
			case '>':
				lexer.step()
				switch lexer.codePoint {
				case '=':
					lexer.step()
					lexer.Token = TRightShiftEqual
				default:
					lexer.Token = TRightShift
				}
			case '=':
				lexer.step()
				lexer.Token = TGreaterEqual
			default:
				lexer.Token = TGreater
			}

		case '<':
			lexer.step()
			switch lexer.codePoint {
			case '<':
				lexer.step()
				switch lexer.codePoint {
				case '=':
					lexer.step()
					lexer.Token = TLeftShiftEqual
				default:
					lexer.Token = TLeftShift
				}
			case '%':
				// Digraph <%
				lexer.step()
				lexer.Token = TCurlyBraceOpen
			case ':':
				// Digraph <:
				lexer.step()
				lexer.Token = TSquareBracketOpen
			case '=':
				lexer.step()
				lexer.Token = TLessEqual
			default:
				lexer.Token = TLess
			}

		case '+':
			lexer.step()
			switch lexer.codePoint {
			case '=':
				lexer.step()
				lexer.Token = TPlusEqual
			case '+':
				lexer.step()
				lexer.Token = TPlusPlus
			default:
				lexer.Token = TPlus
			}

		case '-':
			lexer.step()
			switch lexer.codePoint {
			case '=':
				lexer.step()
				lexer.Token = TMinusEqual
			case '-':
				lexer.step()
				lexer.Token = TMinusMinus
			case '>':
				lexer.step()
				lexer.Token = TPointerDot
			default:
				lexer.Token = TMinus
			}

		case '*':
			lexer.step()
			switch lexer.codePoint {
			case '=':
				lexer.step()
				lexer.Token = TAsteriskEqual
			default:
				lexer.Token = TAsterisk
			}

		case '%':
			lexer.step()
			switch lexer.codePoint {
			case '=':
				lexer.step()
				lexer.Token = TPercentEqual
			case '>':
				// Digraph %>
				lexer.step()
				lexer.Token = TCurlyBraceClose
			default:
				lexer.Token = TPercent
			}

		case '&':
			lexer.step()
			switch lexer.codePoint {
			case '=':
				lexer.step()
				lexer.Token = TAndEqual
			case '&':
				lexer.step()
				lexer.Token = TAndAnd
			default:
				lexer.Token = TAnd
			}

		case '^':
			lexer.step()
			switch lexer.codePoint {
			case '=':
				lexer.step()
				lexer.Token = TXorEqual
			default:
				lexer.Token = TXor
			}

		case '|':
			lexer.step()
			switch lexer.codePoint {
			case '=':
				lexer.step()
				lexer.Token = TOrEqual
			case '|':
				lexer.step()
				lexer.Token = TOrOr
			default:
				lexer.Token = TOr
			}

		case '=':
			lexer.step()
			switch lexer.codePoint {
			case '=':
				lexer.step()
				lexer.Token = TEqualEqual
			default:
				lexer.Token = TEqual
			}

		case '!':
			lexer.step()
			switch lexer.codePoint {
			case '=':
				lexer.step()
				lexer.Token = TNotEqual
			default:
				lexer.Token = TNot
			}

		case ';':
			lexer.step()
			lexer.Token = TSemicolon

		case '{':
			lexer.step()
			lexer.Token = TCurlyBraceOpen

		case '}':
			lexer.step()
			lexer.Token = TCurlyBraceClose

		case ',':
			lexer.step()
			lexer.Token = TComma

		case ':':
			lexer.step()
			switch lexer.codePoint {
			case '>':
				// Digraph :>
				lexer.step()
				lexer.Token = TSquareBracketClose
			default:
				lexer.Token = TColon
			}

		case '(':
			lexer.step()
			lexer.Token = TParenOpen

		case ')':
			lexer.step()
			lexer.Token = TParenClose

		case '[':
			lexer.step()
			lexer.Token = TSquareBracketOpen

		case ']':
			lexer.step()
			lexer.Token = TSquareBracketClose

		case '~':
			lexer.step()
			lexer.Token = TBitNot

		case '?':
			lexer.step()
			lexer.Token = TQuestion

		default:
			// Identifier slow path (UNICODE)
			if xid.Start(lexer.codePoint) {
				lexer.step()
				for xid.Continue(lexer.codePoint) {
					lexer.step()
				}
				// No keywords have non-ASCII characters, so we know it's an identifier.
				lexer.Token = TIdentifier
				lexer.Raw = lexer.raw()
				break
			}

			lexer.end = lexer.current
			lexer.Token = TInvalid
		}

		return
	}
}
