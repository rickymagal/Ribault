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

SHELL := /bin/bash
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
.PHONY: all df ast code tokens images ast-dots ast-images supers clean

all: df ast code supers

GHC_VER   := $(shell $(GHC) --numeric-version)
SHIM_DIR  := build/ghc-shim

.PHONY: supers_prepare
supers_prepare:
	@set -eu
	@echo "[shim] preparando GHC shim em $(SHIM_DIR)"
	@mkdir -p "$(SHIM_DIR)/rts"
	@RTS_DIR="$$(ghc-pkg field rts library-dirs --simple-output)"; \
	BASE_DIR="$$(dirname "$$RTS_DIR")"; \
	RTS_SO="$$(ls "$$BASE_DIR"/libHSrts-*-ghc$(GHC_VER).so 2>/dev/null | head -n1)"; \
	if [ -z "$$RTS_SO" ]; then \
	  RTS_SO="$$(ls "$$RTS_DIR"/libHSrts-*-ghc$(GHC_VER).so 2>/dev/null | head -n1)"; \
	fi; \
	test -n "$$RTS_SO" || { \
	  echo "[shim] ERRO: não achei libHSrts-*-ghc$(GHC_VER).so"; \
	  echo "       Procurei em:"; \
	  echo "         $$BASE_DIR"; \
	  echo "         $$RTS_DIR"; \
	  exit 2; \
	}; \
	ln -sfn "$$RTS_SO" "$(SHIM_DIR)/rts/libHSrts-ghc$(GHC_VER).so"; \
	echo "[shim] RTS → $$RTS_SO"
	@for pkg in base ghc-prim ghc-bignum integer-gmp; do \
	  dir="$$(ghc-pkg field $$pkg library-dirs --simple-output)"; \
	  [ -d "$$dir" ] && ln -sfn "$$dir" "$(SHIM_DIR)/$$(basename "$$dir")" || true; \
	done
	@INC_DIRS="$$(ghc-pkg field rts include-dirs --simple-output)"; \
	CPP=""; CP=""; \
	for d in $$INC_DIRS; do CPP="$$CPP -I$$d"; CP="$$CP:$$d"; done; \
	printf "%s" "$$CPP" > "$(SHIM_DIR)/.cppflags"; \
	printf "%s" "$${CP#:}" > "$(SHIM_DIR)/.cpath"



# ---------- biblioteca de super-instruções --------------------

GHC_RTS_DIR := $(GHC_LIBDIR)/rts
# ----- GHC base ------------------------------------------------
GHC_LIBDIR      := $(shell $(GHC) --print-libdir)         # .../ghc-9.6.6/lib
GHC_VER         := $(shell $(GHC) --numeric-version)
RTS_LIBDIR      := $(shell ghc-pkg field rts library-dirs --simple-output)
RTS_INCDIRS     := $(shell ghc-pkg field rts include-dirs --simple-output)
FEDORA_GHC_BASE := $(dir $(RTS_LIBDIR))                   # .../lib/x86_64-linux-ghc-9.6.6/

# ----- Shim local (sem sudo) ----------------------------------
GHC_SHIM_DIR := build/ghc-shim
GHC_LIBDIR_SHIM := $(GHC_SHIM_DIR)                        # será passado ao script
GHC_RTS_DIR  := $(GHC_LIBDIR_SHIM)/rts

# Includes do RTS (p/ HsFFI.h etc)
CPP_RTS_INCS := $(foreach d,$(RTS_INCDIRS),-I$(d))

# Tudo que a .so pode pedir (cobre layout clássico e Fedora)
DEPS_PATTERNS := \
  rts/libHSrts*.so \
  x86_64-linux-ghc-$(GHC_VER)/libHSrts*.so \
  ghc-prim-*/libHSghc-prim-*.so \
  base-*/libHSbase-*.so \
  integer-gmp-*/libHSinteger-gmp-*.so \
  ghc-bignum-*/libHSghc-bignum-*.so

# (Opcional) libgmp para runtime fechado
GMP_CANDIDATES := /usr/lib64/libgmp.so.10 /usr/lib/libgmp.so.10

# Link sem -threaded (evita *_thr); força noexecstack e rpath curto no bundle local
SUPERS_LINK_FLAGS := -O2 -shared -dynamic -fPIC -no-hs-main \
                     -hide-all-packages \
                     -package base -package ghc-prim -package integer-gmp -package ghc-bignum \
                     -no-user-package-db -package-env - \
                     -optl -Wl,--disable-new-dtags \
                     -optl -Wl,-z,noexecstack \
                     -optl -Wl,-rpath,'$$ORIGIN/ghc-deps:$$ORIGIN' \
                     -optl -Wl,--no-as-needed

# Compila o gerador de supers (supersgen)
$(EXE_SUPERS): $(LEXER_HS) $(PARSER_HS) $(SYNTAX_HS) $(SEMANTIC_HS) \
               $(UNIQUE_HS) $(TYPES_HS) $(PORT_HS) $(NODE_HS) \
               $(SUPERS_EXTRACT) $(SUPERS_EMIT) $(MAIN_SUPERS_HS)
	@echo "[GHC  ] $@"
	@mkdir -p $(EXE_SUPERS).obj $(EXE_SUPERS).hi
	GHC_ENVIRONMENT=- $(GHC) -O2 \
	  -odir $(EXE_SUPERS).obj -hidir $(EXE_SUPERS).hi \
	  -o $@ \
	  $(MAIN_SUPERS_HS) $(LEXER_HS) $(PARSER_HS) \
	  $(SYNTAX_HS) $(SEMANTIC_HS) $(UNIQUE_HS) \
	  $(TYPES_HS) $(PORT_HS) $(NODE_HS) \
	  $(SUPERS_EXTRACT) $(SUPERS_EMIT)

supers: supers_prepare $(EXE_SUPERS)
	@mkdir -p $(SUPERS_DIR)
	@EXE_SUPERS="$(EXE_SUPERS)" \
	 GHC="$(GHC)" \
	 GHC_LIBDIR="$(SHIM_DIR)" \
	 GHC_RTS_DIR="$(SHIM_DIR)/rts" \
	 CPPFLAGS="$$(cat $(SHIM_DIR)/.cppflags)" \
	 C_INCLUDE_PATH="$$(cat $(SHIM_DIR)/.cpath)" \
	 CPATH="$$(cat $(SHIM_DIR)/.cpath)" \
	 SUPERS_DIR="$(SUPERS_DIR)" \
	 DEPS_PATTERNS='$(DEPS_PATTERNS)' \
	 SUPERS_LINK_FLAGS='$(SUPERS_LINK_FLAGS)' \
	 PY_ALIAS="tools/alias_supers.py" \
	 PY_FIX="tools/fix_execstack.py" \
	 GMP_CANDIDATES='$(GMP_CANDIDATES)' \
	 TESTS='$(TESTS)' \
	 bash tools/build_supers.sh




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
	$(GHC) -O2 -package mtl \
	      -odir $(EXE_DF).obj -hidir $(EXE_DF).hi \
	      -o $@ \
	      $(MAIN_DF_HS) $(LEXER_HS) $(PARSER_HS) \
	      $(SYNTAX_HS) $(SEMANTIC_HS) $(UNIQUE_HS) \
	      $(TYPES_HS) $(PORT_HS) $(NODE_HS) \
	      $(BUILDER_HS) $(GRAPHVIZ_HS)

$(EXE_AST): $(LEXER_HS) $(PARSER_HS) $(SYNTAX_HS) $(SEMANTIC_HS) $(ASTGEN_HS) $(MAIN_AST_HS)
	@echo "[GHC  ] $@"
	@mkdir -p $(EXE_AST).obj $(EXE_AST).hi
	$(GHC) -O2 -package mtl \
	      -odir $(EXE_AST).obj -hidir $(EXE_AST).hi \
	      -o $@ \
	      $(MAIN_AST_HS) $(LEXER_HS) $(PARSER_HS) \
	      $(SYNTAX_HS) $(SEMANTIC_HS) $(ASTGEN_HS)

$(EXE_CODE): $(LEXER_HS) $(PARSER_HS) $(SYNTAX_HS) $(SEMANTIC_HS) \
             $(BUILDER_HS) $(CODEGEN_HS) $(MAIN_CODE_HS) $(UNIQUE_HS) \
             $(TYPES_HS) $(PORT_HS) $(NODE_HS)
	@echo "[GHC  ] $@"
	@mkdir -p $(EXE_CODE).obj $(EXE_CODE).hi
	$(GHC) -O2 -package mtl \
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
	@rm -rf $(DF_OUT_DIR) $(AST_OUT_DIR) $(CODE_OUT_DIR) $(SUPERS_DIR) build
