# lambdaflow — Makefile básico do projeto

GHC            := ghc
ALEX           := alex
DOT            := dot

SRC_DIR        := src
TEST_DIR       := test
OUT_DIR        := $(TEST_DIR)/output
DF_IMG_DIR:= $(TEST_DIR)/df-images

SYNTAX_HS      := $(SRC_DIR)/Syntax.hs
SEMANTIC_HS    := $(SRC_DIR)/Semantic.hs
LEXER_SRC      := $(SRC_DIR)/Lexer.x
LEXER_HS       := $(SRC_DIR)/Lexer.hs
MAIN_HS        := $(SRC_DIR)/Main.hs
PARSER_SRC     := $(SRC_DIR)/Parser.y
PARSER_HS      := $(SRC_DIR)/Parser.hs
GRAPHGEN_HS      := $(SRC_DIR)/Graph-gen.hs
EXE            := analysis

TESTS          := $(wildcard $(TEST_DIR)/*.hsk)
DOTS           := $(patsubst $(TEST_DIR)/%.hsk, $(OUT_DIR)/%.dot, $(TESTS))
IMAGES         := $(patsubst $(OUT_DIR)/%.dot, $(DF_IMG_DIR)/%.png, $(DOTS))

.PHONY: all tokens images clean run

# Regra principal: gera todos os .dot e as imagens
all: tokens images

# Cria diretórios de saída
$(OUT_DIR):
	@mkdir -p $@

$(DF_IMG_DIR):
	@mkdir -p $@

# Gera Lexer.hs a partir de Lexer.x
$(LEXER_HS): $(LEXER_SRC)
	@echo [ALEX] $<
	$(ALEX) $<

# Gera Parser.hs a partir de Parser.y
$(PARSER_HS): $(PARSER_SRC)
	@echo [HAPPY] $<
	happy --ghc -o $@ $<

# Compila o binário principal
$(EXE): $(LEXER_HS) $(PARSER_HS) $(SYNTAX_HS) $(SEMANTIC_HS) $(GRAPHGEN_HS) $(MAIN_HS)
	@echo [GHC ] $@
	$(GHC) -o $(EXE) $(MAIN_HS) $(LEXER_HS) $(PARSER_HS) $(SYNTAX_HS) $(SEMANTIC_HS) $(GRAPHGEN_HS)

# Para cada .hsk, gera um .dot correspondente
$(OUT_DIR)/%.dot: $(TEST_DIR)/%.hsk | $(EXE) $(OUT_DIR)
	@echo [DOT ] $< → $@
	./$(EXE) $< > $@

# Para cada .dot, gera uma .png em ast-images
$(DF_IMG_DIR)/%.png: $(OUT_DIR)/%.dot | $(DF_IMG_DIR)
	@echo [IMG ] $< → $@
	$(DOT) -Tpng $< -o $@

# Target geral para gerar todos os .dot
tokens: $(DOTS)

# Target geral para gerar todas as imagens
images: $(IMAGES)

# Execução manual via stdin
run: $(EXE)
	@echo "Digite código e pressione Ctrl-D:"
	./$(EXE)

# Limpeza de artefatos
clean:
	@rm -f $(SRC_DIR)/*.hi $(SRC_DIR)/*.o $(EXE)
	@rm -f $(LEXER_HS)
	@rm -f $(PARSER_HS)
	@rm -rf $(OUT_DIR)
	@echo "Limpeza completa."
