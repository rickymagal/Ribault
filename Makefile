# ------------------------------------------------------------
# HTC – Makefile com df, ast e codegen
# ------------------------------------------------------------
GHC         := ghc
ALEX        := alex
HAPPY       := happy
DOT         := dot

# -------- pastas ------------------------------------------------
SRC_DIR     := src
TEST_DIR    := test

# Saídas
DF_OUT_DIR  := $(TEST_DIR)/output
DF_IMG_DIR  := $(TEST_DIR)/df-images
AST_OUT_DIR := $(TEST_DIR)/ast-output
AST_IMG_DIR := $(TEST_DIR)/ast-images
TALM_DIR    := $(TEST_DIR)/talm

# -------- fontes -----------------------------------------------
LEXER_SRC   := $(SRC_DIR)/Lexer.x
PARSER_SRC  := $(SRC_DIR)/Parser.y
LEXER_HS    := $(SRC_DIR)/Lexer.hs
PARSER_HS   := $(SRC_DIR)/Parser.hs
SYNTAX_HS   := $(SRC_DIR)/Syntax.hs
SEMANTIC_HS := $(SRC_DIR)/Semantic.hs
GRAPHGEN_HS := $(SRC_DIR)/Graph-gen.hs
ASTGEN_HS   := $(SRC_DIR)/AST-gen.hs
CODEGEN_HS  := $(SRC_DIR)/Codegen.hs

MAIN_DF_HS  := $(SRC_DIR)/MainGraph.hs    # Dataflow generator
MAIN_AST_HS := $(SRC_DIR)/MainAST.hs      # AST generator
MAIN_CG_HS  := $(SRC_DIR)/MainCode.hs     # Codegen

# -------- executáveis -----------------------------------------
EXE_DF      := analysis         # gera dataflow .dot
EXE_AST     := analysis-ast     # gera AST .dot
EXE_CG      := codegen          # gera TALM .talm

# ============================================================
# Casos de teste
# ============================================================
TESTS       := $(wildcard $(TEST_DIR)/*.hsk)

# Dataflow artifacts
DF_DOTS     := $(patsubst $(TEST_DIR)/%.hsk,$(DF_OUT_DIR)/%.dot,$(TESTS))
DF_IMGS     := $(patsubst $(DF_OUT_DIR)/%.dot,$(DF_IMG_DIR)/%.png,$(DF_DOTS))
DF_TALMS    := $(patsubst $(DF_OUT_DIR)/%.dot,$(TALM_DIR)/%.fl,$(DF_DOTS))

# AST artifacts
AST_DOTS    := $(patsubst $(TEST_DIR)/%.hsk,$(AST_OUT_DIR)/%.dot,$(TESTS))
AST_IMGS    := $(patsubst $(AST_OUT_DIR)/%.dot,$(AST_IMG_DIR)/%.png,$(AST_DOTS))

# ------------------------------------------------------------
# Alvos de alto nível
# ------------------------------------------------------------
.PHONY: all df ast build-codegen tokens images talms ast-dots ast-images clean run

all: df ast build-codegen talms        ## gera .dot, .png e .talm

# Dataflow
df: tokens images talms    ## gera dataflow outputs

tokens: $(DF_DOTS)         ## gera dataflow .dot
images: $(DF_IMGS)         ## png dataflow

talms: $(DF_TALMS)         ## gera TALM a partir do dataflow .dot

# AST
ast: ast-dots ast-images   ## gera AST outputs

ast-dots: $(AST_DOTS)      ## gera AST .dot
ast-images: $(AST_IMGS)    ## png AST

# Compila gerador TALM (binário)
build-codegen: $(EXE_CG)

# Geração de TALM a partir dos .dot dataflow
talms: $(DF_TALMS)

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
# Compilação executáveis
# ------------------------------------------------------------
$(EXE_DF): $(LEXER_HS) $(PARSER_HS) $(SYNTAX_HS) $(SEMANTIC_HS) $(GRAPHGEN_HS) $(MAIN_DF_HS)
	@echo "[GHC  ] $@"
	$(GHC) -O2 -o $@ $(MAIN_DF_HS) $(LEXER_HS) $(PARSER_HS) $(SYNTAX_HS) $(SEMANTIC_HS) $(GRAPHGEN_HS)

$(EXE_AST): $(LEXER_HS) $(PARSER_HS) $(SYNTAX_HS) $(SEMANTIC_HS) $(ASTGEN_HS) $(MAIN_AST_HS)
	@echo "[GHC  ] $@"
	$(GHC) -O2 -o $@ $(MAIN_AST_HS) $(LEXER_HS) $(PARSER_HS) $(SYNTAX_HS) $(SEMANTIC_HS) $(ASTGEN_HS)

$(EXE_CG): $(CODEGEN_HS) $(MAIN_CG_HS)
	@echo "[GHC  ] $@"
	$(GHC) -O2 -o $@ $(MAIN_CG_HS) $(CODEGEN_HS)

# ------------------------------------------------------------
# Geração .dot dataflow
# ------------------------------------------------------------
$(DF_OUT_DIR)/%.dot: $(TEST_DIR)/%.hsk | $(EXE_DF)
	@mkdir -p $(DF_OUT_DIR)
	@echo "[DOT-DF] $< → $@"
	./$(EXE_DF) $< > $@

# ------------------------------------------------------------
# Geração .dot AST
# ------------------------------------------------------------
$(AST_OUT_DIR)/%.dot: $(TEST_DIR)/%.hsk | $(EXE_AST)
	@mkdir -p $(AST_OUT_DIR)
	@echo "[DOT-AST] $< → $@"
	./$(EXE_AST) $< > $@

# ------------------------------------------------------------
# Renderização PNG dataflow
# ------------------------------------------------------------
$(DF_IMG_DIR)/%.png: $(DF_OUT_DIR)/%.dot | $(DF_IMG_DIR)
	@mkdir -p $(DF_IMG_DIR)
	@echo "[IMG-DF] $< → $@"
	$(DOT) -Tpng $< -o $@

# ------------------------------------------------------------
# Renderização PNG AST
# ------------------------------------------------------------
$(AST_IMG_DIR)/%.png: $(AST_OUT_DIR)/%.dot | $(AST_IMG_DIR)
	@mkdir -p $(AST_IMG_DIR)
	@echo "[IMG-AST] $< → $@"
	$(DOT) -Tpng $< -o $@

# ------------------------------------------------------------
# ------------------------------------------------------------
# TALM via codegen
$(TALM_DIR)/%.fl: $(DF_OUT_DIR)/%.dot | $(EXE_CG) $(TALM_DIR)
	@mkdir -p $(TALM_DIR)
	@echo "[TALM ] $< → $@"
	./$(EXE_CG) < $< > $@  # generate TALM file $< > $@
	@echo "[TALM ] $< → $@"
	./$(EXE_CG) < $< > $@

# ------------------------------------------------------------
# Criação de diretórios intermediários
# ------------------------------------------------------------
$(DF_OUT_DIR) $(DF_IMG_DIR) $(AST_OUT_DIR) $(AST_IMG_DIR) $(TALM_DIR):
	@mkdir -p $@

# ------------------------------------------------------------
# Execução manual stdin
# ------------------------------------------------------------
# ------------------------------------------------------------
run: $(EXE_DF)
	@echo "Digite código e Ctrl-D:" 
	./$(EXE_DF)

# ------------------------------------------------------------
# Generate Haddock documentation
# ------------------------------------------------------------

DOCS_DIR := docs/html

.PHONY: docs
docs: $(DOCS_DIR)
	haddock \
	  --html \
	  --hoogle \
	  --hyperlinked-source \
	  --title="HTC API Documentation" \
	  --odir=$(DOCS_DIR) \
	  src/Lexer.hs \
	  src/Parser.hs \
	  src/Syntax.hs \
	  src/Semantic.hs \
	  src/AST-gen.hs \
	  src/Graph-gen.hs \
	  src/Codegen.hs

$(DOCS_DIR):
	mkdir -p $(DOCS_DIR)

# ------------------------------------------------------------
# Limpeza
# ------------------------------------------------------------
clean:
	@echo "Limpeza completa."
	@rm -f $(SRC_DIR)/*.hi $(SRC_DIR)/*.o $(EXE_DF) $(EXE_AST) $(EXE_CG)
	@rm -f $(LEXER_HS) $(PARSER_HS)
	@rm -rf $(DF_OUT_DIR)  $(AST_OUT_DIR) 
