# ------------------------------------------------------------
# Ribault – Makefile com df, ast e libsupers
# ------------------------------------------------------------
GHC         := ghc
ALEX        := alex
HAPPY       := happy
DOT         := dot

# -------- pastas ------------------------------------------------
SRC_DIR     := src
TEST_DIR    := test

# Saídas
DF_OUT_DIR  := $(TEST_DIR)/df-output
DF_IMG_DIR  := $(TEST_DIR)/df-images
AST_OUT_DIR := $(TEST_DIR)/ast-output
AST_IMG_DIR := $(TEST_DIR)/ast-images
CODE_OUT_DIR := $(TEST_DIR)/talm

# -------- fontes -----------------------------------------------
LEXER_SRC   := $(SRC_DIR)/Analysis/Lexer.x
PARSER_SRC  := $(SRC_DIR)/Analysis/Parser.y
LEXER_HS    := $(SRC_DIR)/Analysis/Lexer.hs
PARSER_HS   := $(SRC_DIR)/Analysis/Parser.hs
SYNTAX_HS   := $(SRC_DIR)/Analysis/Syntax.hs
SEMANTIC_HS := $(SRC_DIR)/Analysis/Semantic.hs
BUILDER_HS  := $(SRC_DIR)/Synthesis/Builder.hs
GRAPHVIZ_HS := $(SRC_DIR)/Synthesis/GraphViz.hs
ASTGEN_HS   := $(SRC_DIR)/Analysis/AST-gen.hs
CODEGEN_HS  := $(SRC_DIR)/Synthesis/Codegen.hs
UNIQUE_HS   := $(SRC_DIR)/Synthesis/Unique.hs
TYPES_HS    := $(SRC_DIR)/Synthesis/Types.hs
SSA_HS      := $(SRC_DIR)/Synthesis/SSA.hs
PORT_HS     := $(SRC_DIR)/Synthesis/Port.hs
NODE_HS     := $(SRC_DIR)/Synthesis/Node.hs

MAIN_CODE_HS := $(SRC_DIR)/Synthesis/MainCode.hs
MAIN_DF_HS  := $(SRC_DIR)/Synthesis/MainGraph.hs
MAIN_AST_HS := $(SRC_DIR)/Analysis/MainAST.hs

# → novas super-instruções em Haskell
SUPERS_HS   := $(SRC_DIR)/Lib/Supers.hs
LIB_SUPERS  := libsupers.so

# -------- executáveis -----------------------------------------
EXE_DF      := synthesis
EXE_AST     := analysis
EXE_CODE     := codegen

# ============================================================
# Casos de teste
# ============================================================
TESTS       := $(wildcard $(TEST_DIR)/*.hsk)

DF_DOTS     := $(patsubst $(TEST_DIR)/%.hsk,$(DF_OUT_DIR)/%.dot,$(TESTS))
DF_IMGS     := $(patsubst $(DF_OUT_DIR)/%.dot,$(DF_IMG_DIR)/%.png,$(DF_DOTS))

AST_DOTS    := $(patsubst $(TEST_DIR)/%.hsk,$(AST_OUT_DIR)/%.dot,$(TESTS))
AST_IMGS    := $(patsubst $(AST_OUT_DIR)/%.dot,$(AST_IMG_DIR)/%.png,$(AST_DOTS))

CODE_FL  := $(patsubst $(TEST_DIR)/%.hsk,$(CODE_OUT_DIR)/%.fl,$(TESTS))
# ------------------------------------------------------------
# Alvos de alto nível
# ------------------------------------------------------------
.PHONY: all df ast code tokens images ast-dots ast-images supers clean

all: df ast code supers       ## gera .dot e .png de Dataflow/AST e libsupers.so

# ---------- biblioteca de super-instruções --------------------
supers: $(LIB_SUPERS)     ## compila libsupers.so

$(LIB_SUPERS): $(SUPERS_HS)
	@echo "[GHC  ] $@"
	$(GHC) -O2 -shared -dynamic -fPIC                     \
	       -package hint -package hashable               \
	       -o $@ $<

# ---------- Dataflow & AST pipelines -------------------------
df: tokens images        
tokens: $(DF_DOTS)
images: $(DF_IMGS)

ast: ast-dots ast-images 
ast-dots: $(AST_DOTS)
ast-images: $(AST_IMGS)

code: $(CODE_FL)  

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
$(EXE_DF): $(LEXER_HS) $(PARSER_HS) $(SYNTAX_HS) $(SEMANTIC_HS) \
           $(BUILDER_HS) $(GRAPHVIZ_HS) $(MAIN_DF_HS) $(UNIQUE_HS) \
	   $(TYPES_HS) $(SSA_HS) $(PORT_HS) $(NODE_HS) 
	@echo "[GHC  ] $@"
	$(GHC) -O2 -o $@ $(MAIN_DF_HS) $(LEXER_HS) $(PARSER_HS) \
	                 $(SYNTAX_HS) $(SEMANTIC_HS) $(UNIQUE_HS) \
	                 $(TYPES_HS) $(SSA_HS) $(PORT_HS) $(NODE_HS) \
	                 $(BUILDER_HS) $(GRAPHVIZ_HS)

$(EXE_AST): $(LEXER_HS) $(PARSER_HS) $(SYNTAX_HS) $(SEMANTIC_HS) $(ASTGEN_HS) $(MAIN_AST_HS)
	@echo "[GHC  ] $@"
	$(GHC) -O2 -o $@ $(MAIN_AST_HS) $(LEXER_HS) $(PARSER_HS) $(SYNTAX_HS) $(SEMANTIC_HS) $(ASTGEN_HS)

$(EXE_CODE): $(LEXER_HS) $(PARSER_HS) $(SYNTAX_HS) $(SEMANTIC_HS) \
             $(BUILDER_HS) $(CODEGEN_HS) $(MAIN_CODE_HS) $(UNIQUE_HS) \
	     $(TYPES_HS) $(SSA_HS) $(PORT_HS) $(NODE_HS)
	@echo "[GHC  ] $@"
	$(GHC) -O2 -o $@ $(MAIN_CODE_HS) $(LEXER_HS) $(PARSER_HS) \
	                 $(SYNTAX_HS) $(SEMANTIC_HS) \
	                 $(BUILDER_HS)  $(CODEGEN_HS) $(UNIQUE_HS) \
	   		 $(TYPES_HS) $(SSA_HS) $(PORT_HS) $(NODE_HS)

# ------------------------------------------------------------
# Dataflow .dot
$(DF_OUT_DIR)/%.dot: $(TEST_DIR)/%.hsk | $(EXE_DF)
	@mkdir -p $(DF_OUT_DIR)
	@echo "[DOT-DF] $< → $@"
	./$(EXE_DF) $< > $@

# ------------------------------------------------------------
# PNG dataflow
$(DF_IMG_DIR)/%.png: $(DF_OUT_DIR)/%.dot
	@mkdir -p $(DF_IMG_DIR)
	@echo "[IMG-DF] $< → $@"
	$(DOT) -Tpng $< -o $@

# ------------------------------------------------------------
# AST .dot
$(AST_OUT_DIR)/%.dot: $(TEST_DIR)/%.hsk | $(EXE_AST)
	@mkdir -p $(AST_OUT_DIR)
	@echo "[DOT-AST] $< → $@"
	./$(EXE_AST) $< > $@

# ------------------------------------------------------------
# PNG AST
$(AST_IMG_DIR)/%.png: $(AST_OUT_DIR)/%.dot
	@mkdir -p $(AST_IMG_DIR)
	@echo "[IMG-AST] $< → $@"
	$(DOT) -Tpng $< -o $@

# ------------------------------------------------------------

$(CODE_OUT_DIR)/%.fl: $(TEST_DIR)/%.hsk | $(EXE_CODE)
	@mkdir -p $(CODE_OUT_DIR)
	@echo "[TALM ] $< → $@"
	./$(EXE_CODE) $< > $@

# limpeza
clean:
	@echo "Limpeza completa."
	@rm -f $(SRC_DIR)/Analysis/*.hi $(SRC_DIR)/Analysis/*.o \
	        $(SRC_DIR)/Synthesis/*.hi $(SRC_DIR)/Synthesis/*.o \
	        $(EXE_DF) $(EXE_AST) $(EXE_CODE) $(LIB_SUPERS)
	@rm -f $(LEXER_HS) $(PARSER_HS)
	@rm -rf $(DF_OUT_DIR) $(AST_OUT_DIR)
