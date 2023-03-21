package main

import (
	"flag"
	"log"
	"os"
	"path/filepath"

	"github.com/jchv/chiru/internal/lexer"
)

func main() {
	flag.Parse()
	for _, path := range flag.Args() {
		_, file := filepath.Split(path)
		source, err := os.ReadFile(path)
		if err != nil {
			log.Fatalf("Error reading %v: %v", path, err)
		}
		l := lexer.NewLexer(string(source))
		for {
			l.Next()
			if l.Token == lexer.TEndOfSource {
				break
			}
			log.Printf("%s:%s %s\n", file, l.Token, l.Raw)
			if l.Token == lexer.TInvalid {
				break
			}
		}
	}
}
