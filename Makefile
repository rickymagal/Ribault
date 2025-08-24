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
DF_OUT_DIR   := $(TEST_DIR)/df-output
DF_IMG_DIR   := $(TEST_DIR)/df-images
AST_OUT_DIR  := $(TEST_DIR)/ast-output
AST_IMG_DIR  := $(TEST_DIR)/ast-images
CODE_OUT_DIR := $(TEST_DIR)/talm

# -------- fontes -----------------------------------------------
LEXER_SRC    := $(SRC_DIR)/Analysis/Lexer.x
PARSER_SRC   := $(SRC_DIR)/Analysis/Parser.y
LEXER_HS     := $(SRC_DIR)/Analysis/Lexer.hs
PARSER_HS    := $(SRC_DIR)/Analysis/Parser.hs
SYNTAX_HS    := $(SRC_DIR)/Analysis/Syntax.hs
SEMANTIC_HS  := $(SRC_DIR)/Analysis/Semantic.hs
BUILDER_HS   := $(SRC_DIR)/Synthesis/Builder.hs
GRAPHVIZ_HS  := $(SRC_DIR)/Synthesis/GraphViz.hs
ASTGEN_HS    := $(SRC_DIR)/Analysis/AST-gen.hs
CODEGEN_HS   := $(SRC_DIR)/Synthesis/Codegen.hs
UNIQUE_HS    := $(SRC_DIR)/Synthesis/Unique.hs
TYPES_HS     := $(SRC_DIR)/Synthesis/Types.hs
PORT_HS      := $(SRC_DIR)/Synthesis/Port.hs
NODE_HS      := $(SRC_DIR)/Synthesis/Node.hs

MAIN_CODE_HS := $(SRC_DIR)/Synthesis/MainCode.hs
MAIN_DF_HS   := $(SRC_DIR)/Synthesis/MainGraph.hs
MAIN_AST_HS  := $(SRC_DIR)/Analysis/MainAST.hs

MAIN_SUPERS_HS := $(SRC_DIR)/Synthesis/MainSupers.hs
EXE_SUPERS     := supersgen
SUPERS_EXTRACT := $(SRC_DIR)/Synthesis/SuperExtract.hs
SUPERS_EMIT    := $(SRC_DIR)/Synthesis/SupersEmit.hs
SUPERS_DIR     := $(TEST_DIR)/supers

# -------- executáveis -----------------------------------------
EXE_DF       := synthesis
EXE_AST      := analysis
EXE_CODE     := codegen

# ============================================================
# Casos de teste
# ============================================================
TESTS        := $(wildcard $(TEST_DIR)/*.hsk)

DF_DOTS      := $(patsubst $(TEST_DIR)/%.hsk,$(DF_OUT_DIR)/%.dot,$(TESTS))
DF_IMGS      := $(patsubst $(DF_OUT_DIR)/%.dot,$(DF_IMG_DIR)/%.png,$(DF_DOTS))

AST_DOTS     := $(patsubst $(TEST_DIR)/%.hsk,$(AST_OUT_DIR)/%.dot,$(TESTS))
AST_IMGS     := $(patsubst $(AST_OUT_DIR)/%.dot,$(AST_IMG_DIR)/%.png,$(AST_DOTS))

CODE_FL      := $(patsubst $(TEST_DIR)/%.hsk,$(CODE_OUT_DIR)/%.fl,$(TESTS))

# ------------------------------------------------------------
# Alvos de alto nível
# ------------------------------------------------------------
.PHONY: all df ast code tokens images ast-dots ast-images clean supers

all: df ast code supers 

# ---------- biblioteca de super-instruções --------------------
supers: $(EXE_SUPERS)
	@mkdir -p $(SUPERS_DIR)
	@set -e; \
	for f in $(TESTS); do \
	  base=$$(basename $$f .hsk); \
	  outdir=$(SUPERS_DIR)/$$base; \
	  mkdir -p $$outdir; \
	  echo "[SUPERS] $$f → $$outdir/Supers.hs"; \
	  ./$(EXE_SUPERS) $$f > $$outdir/Supers.hs.tmp; \
	  if [ -s $$outdir/Supers.hs.tmp ]; then \
	    mv $$outdir/Supers.hs.tmp $$outdir/Supers.hs; \
	    echo "[GHC  ] $$outdir/libsupers.so"; \
	    $(GHC) -O2 -shared -dynamic -fPIC -threaded \
	           -o $$outdir/libsupers.so $$outdir/Supers.hs; \
	  else \
	    rm -f $$outdir/Supers.hs.tmp $$outdir/Supers.hs $$outdir/libsupers.so; \
	  fi; \
	done

$(EXE_SUPERS): $(LEXER_HS) $(PARSER_HS) $(SYNTAX_HS) $(SEMANTIC_HS) \
               $(UNIQUE_HS) $(TYPES_HS) $(PORT_HS) $(NODE_HS) \
               $(SRC_DIR)/Synthesis/SuperExtract.hs \
               $(SRC_DIR)/Synthesis/SupersEmit.hs \
               $(MAIN_SUPERS_HS)
	@echo "[GHC  ] $@"
	@mkdir -p $(EXE_SUPERS).obj $(EXE_SUPERS).hi
	$(GHC) -O2 \
	      -odir $(EXE_SUPERS).obj -hidir $(EXE_SUPERS).hi \
	      -o $@ \
	      $(MAIN_SUPERS_HS) $(LEXER_HS) $(PARSER_HS) \
	      $(SYNTAX_HS) $(SEMANTIC_HS) $(UNIQUE_HS) \
	      $(TYPES_HS) $(PORT_HS) $(NODE_HS) \
	      $(SRC_DIR)/Synthesis/SuperExtract.hs \
	      $(SRC_DIR)/Synthesis/SupersEmit.hs


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
           $(TYPES_HS) $(PORT_HS) $(NODE_HS)
	@echo "[GHC  ] $@"
	@mkdir -p $(EXE_DF).obj $(EXE_DF).hi
	$(GHC) -O2 \
	      -odir $(EXE_DF).obj -hidir $(EXE_DF).hi \
	      -o $@ \
	      $(MAIN_DF_HS) $(LEXER_HS) $(PARSER_HS) \
	      $(SYNTAX_HS) $(SEMANTIC_HS) $(UNIQUE_HS) \
	      $(TYPES_HS) $(PORT_HS) $(NODE_HS) \
	      $(BUILDER_HS) $(GRAPHVIZ_HS)

$(EXE_AST): $(LEXER_HS) $(PARSER_HS) $(SYNTAX_HS) $(SEMANTIC_HS) $(ASTGEN_HS) $(MAIN_AST_HS)
	@echo "[GHC  ] $@"
	@mkdir -p $(EXE_AST).obj $(EXE_AST).hi
	$(GHC) -O2 \
	      -odir $(EXE_AST).obj -hidir $(EXE_AST).hi \
	      -o $@ \
	      $(MAIN_AST_HS) $(LEXER_HS) $(PARSER_HS) \
	      $(SYNTAX_HS) $(SEMANTIC_HS) $(ASTGEN_HS)

$(EXE_CODE): $(LEXER_HS) $(PARSER_HS) $(SYNTAX_HS) $(SEMANTIC_HS) \
             $(BUILDER_HS) $(CODEGEN_HS) $(MAIN_CODE_HS) $(UNIQUE_HS) \
             $(TYPES_HS) $(PORT_HS) $(NODE_HS)
	@echo "[GHC  ] $@"
	@mkdir -p $(EXE_CODE).obj $(EXE_CODE).hi
	$(GHC) -O2 \
	      -odir $(EXE_CODE).obj -hidir $(EXE_CODE).hi \
	       -o $@ \
	      $(MAIN_CODE_HS) $(LEXER_HS) $(PARSER_HS) \
	      $(SYNTAX_HS) $(SEMANTIC_HS) \
	      $(BUILDER_HS) $(CODEGEN_HS) $(UNIQUE_HS) \
	      $(TYPES_HS) $(PORT_HS) $(NODE_HS)

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
# TALM assembly
$(CODE_OUT_DIR)/%.fl: $(TEST_DIR)/%.hsk | $(EXE_CODE)
	@mkdir -p $(CODE_OUT_DIR)
	@echo "[TALM ] $< → $@"
	./$(EXE_CODE) $< > $@

# limpeza
clean:
	@echo "Limpeza completa."
	@rm -rf synthesis.obj synthesis.hi \
	        analysis.obj analysis.hi   \
	        codegen.obj codegen.hi     \
	        $(SRC_DIR)/Analysis/*.hi $(SRC_DIR)/Analysis/*.o \
	        $(SRC_DIR)/Synthesis/*.hi $(SRC_DIR)/Synthesis/*.o \
	        $(EXE_DF) $(EXE_AST) $(EXE_CODE) $(EXE_SUPERS) \
		$(EXE_SUPERS).obj $(EXE_SUPERS).hi 
	@rm -f $(LEXER_HS) $(PARSER_HS)
	@rm -rf $(DF_OUT_DIR) $(AST_OUT_DIR) $(CODE_OUT_DIR)
