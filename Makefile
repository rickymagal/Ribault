# ------------------------------------------------------------
# HTC – Makefile com dois modos: df (padrão) e ast
# ------------------------------------------------------------
GHC   := ghc
ALEX  := alex
HAPPY := happy
DOT   := dot

# -------- pastas ------------------------------------------------
SRC_DIR  := src
TEST_DIR := test

# -------- fontes fixas -----------------------------------------
LEXER_SRC   := $(SRC_DIR)/Lexer.x
PARSER_SRC  := $(SRC_DIR)/Parser.y
LEXER_HS    := $(SRC_DIR)/Lexer.hs
PARSER_HS   := $(SRC_DIR)/Parser.hs
SYNTAX_HS   := $(SRC_DIR)/Syntax.hs
SEMANTIC_HS := $(SRC_DIR)/Semantic.hs
GRAPHGEN_HS := $(SRC_DIR)/Graph-gen.hs
ASTGEN_HS   := $(SRC_DIR)/AST-gen.hs
MAIN_DF_HS  := $(SRC_DIR)/Main.hs       # usa GraphGen → data-flow
MAIN_AST_HS := $(SRC_DIR)/MainAST.hs    # usa ASTGen  → AST

# -------- executáveis ------------------------------------------
EXE_DF  := analysis         # gera grafo data-flow  (.df)
EXE_AST := analysis-ast     # gera grafo AST        (.ast)

# ------------------------------------------------------------
# Escolha de modo: df (default) ou ast
# ------------------------------------------------------------
MODE ?= df      # use: make MODE=ast

ifeq ($(MODE),ast)
  OUT_DIR := $(TEST_DIR)/ast-output
  IMG_DIR := $(TEST_DIR)/ast-images
  TARGET_EXE  := $(EXE_AST)
  TARGET_MAIN := $(MAIN_AST_HS)
  EXTRA_SRCS  := $(ASTGEN_HS)
else
  OUT_DIR := $(TEST_DIR)/output
  IMG_DIR := $(TEST_DIR)/df-images
  TARGET_EXE  := $(EXE_DF)
  TARGET_MAIN := $(MAIN_DF_HS)
  EXTRA_SRCS  := $(GRAPHGEN_HS)
endif

# ============================================================
# CASOS DE TESTE e artefatos gerados
# ============================================================
TESTS   := $(wildcard $(TEST_DIR)/*.hsk)
DOTS    := $(patsubst $(TEST_DIR)/%.hsk,$(OUT_DIR)/%.dot,$(TESTS))
IMAGES  := $(patsubst $(OUT_DIR)/%.dot,$(IMG_DIR)/%.png,$(DOTS))

# ------------------------------------------------------------
# Alvos de alto nível
# ------------------------------------------------------------
.PHONY: all tokens images clean run

all: tokens images          ## gera todos os .dot e PNGs

tokens: $(DOTS)             ## gera todos os .dot
images: $(IMAGES)           ## renderiza todos os PNGs

# ------------------------------------------------------------
# Geração de Lexer / Parser
# ------------------------------------------------------------
$(LEXER_HS): $(LEXER_SRC)
	@echo "[ALEX ] $<"
	$(ALEX) $<

$(PARSER_HS): $(PARSER_SRC)
	@echo "[HAPPY] $<"
	$(HAPPY) --ghc -o $@ $<

# ------------------------------------------------------------
# Compilação do executável de acordo com MODE
# ------------------------------------------------------------
$(EXE_DF):  $(LEXER_HS) $(PARSER_HS) $(SYNTAX_HS) \
            $(SEMANTIC_HS) $(GRAPHGEN_HS) $(MAIN_DF_HS)
	@echo "[GHC  ] $@"
	$(GHC) -O2 -o $@ $(MAIN_DF_HS) $(LEXER_HS) $(PARSER_HS) \
                       $(SYNTAX_HS) $(SEMANTIC_HS) $(GRAPHGEN_HS)

$(EXE_AST): $(LEXER_HS) $(PARSER_HS) $(SYNTAX_HS) \
            $(SEMANTIC_HS) $(ASTGEN_HS) $(MAIN_AST_HS)
	@echo "[GHC  ] $@"
	$(GHC) -O2 -o $@ $(MAIN_AST_HS) $(LEXER_HS) $(PARSER_HS) \
                       $(SYNTAX_HS) $(SEMANTIC_HS) $(ASTGEN_HS)

# ------------------------------------------------------------
# Geração dos .dot a partir dos .hsk
# ------------------------------------------------------------
$(OUT_DIR)/%.dot: $(TEST_DIR)/%.hsk | $(TARGET_EXE) $(OUT_DIR)
	@echo "[DOT  ] $< → $@"
	./$(TARGET_EXE) $< > $@

# ------------------------------------------------------------
# Renderização PNG
# ------------------------------------------------------------
$(IMG_DIR)/%.png: $(OUT_DIR)/%.dot | $(IMG_DIR)
	@echo "[IMG  ] $< → $@"
	$(DOT) -Tpng $< -o $@

# ------------------------------------------------------------
# Diretórios intermediários
# ------------------------------------------------------------
$(OUT_DIR) $(IMG_DIR):
	@mkdir -p $@

# ------------------------------------------------------------
# Execução manual via stdin (respeita MODE)
# ------------------------------------------------------------
run: $(TARGET_EXE)
	@echo "Digite código e pressione Ctrl-D:"
	./$(TARGET_EXE)

# ------------------------------------------------------------
# Limpeza
# ------------------------------------------------------------
clean:
	@echo "Limpeza completa."
	@rm -f $(SRC_DIR)/*.hi $(SRC_DIR)/*.o $(EXE_DF) $(EXE_AST)
	@rm -f $(LEXER_HS) $(PARSER_HS)
	@rm -rf $(TEST_DIR)/output $(TEST_DIR)/ast-output
	@echo
