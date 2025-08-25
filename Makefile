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
.PHONY: all df ast code tokens images ast-dots ast-images clean

all: df ast code supers 

# ---------- biblioteca de super-instruções --------------------
GHC_LIBDIR   := $(shell $(GHC) --print-libdir)
GHC_RTS_DIR  := $(GHC_LIBDIR)/rts

# libs runtime Haskell a empacotar (TODAS as NEEDED Haskell + RTS)
DEPS_PATTERNS := rts/libHSrts*.so \
                 ghc-prim-*/libHSghc-prim-*.so \
                 base-*/libHSbase-*.so \
                 integer-gmp-*/libHSinteger-gmp-*.so \
                 ghc-bignum-*/libHSghc-bignum-*.so

# libgmp do sistema (opcional; fecha NEEDED sem depender de /usr/lib em runtime)
GMP_CANDIDATES := /usr/lib64/libgmp.so.10 /usr/lib/libgmp.so.10

# detecta o RTS NÃO-THREADED (evita depender do *_thr)
RTS_BASE_SO  := $(firstword $(wildcard $(GHC_RTS_DIR)/libHSrts-ghc*.so))
RTS_BASE_LIB := $(patsubst lib%.so,%,$(notdir $(RTS_BASE_SO)))

# flags de link: ambiente limpo + RPATH curto (transitivo) — sem -threaded
SUPERS_LINK_FLAGS := -O2 -shared -dynamic -fPIC -no-hs-main \
                     -hide-all-packages -package base -package ghc-prim -package integer-gmp -package ghc-bignum \
                     -no-user-package-db -package-env - \
                     -optl -Wl,--disable-new-dtags \
                     -optl -Wl,-rpath,'$$ORIGIN/ghc-deps:$$ORIGIN' \
                     -optl -Wl,--no-as-needed

# script python para REMOVER o bit EXEC do PT_GNU_STACK em todas as .so (sem execstack)
PY_FIX := $(SUPERS_DIR)/fix_execstack.py
$(PY_FIX):
	@mkdir -p $(SUPERS_DIR)
	@printf '%s\n' "#!/usr/bin/env python3" > $(PY_FIX)
	@printf '%s\n' "# remove EXEC do PT_GNU_STACK em ELF64 little-endian" >> $(PY_FIX)
	@printf '%s\n' "import sys,struct" >> $(PY_FIX)
	@printf '%s\n' "if len(sys.argv)<2:print('uso: fix_execstack.py <lib.so>...');sys.exit(1)" >> $(PY_FIX)
	@printf '%s\n' "for p in sys.argv[1:]:" >> $(PY_FIX)
	@printf '%s\n' "  with open(p,'r+b') as f: data=bytearray(f.read())" >> $(PY_FIX)
	@printf '%s\n' "  if data[:4]!=b'\\x7fELF': print('[SKIP]',p); continue" >> $(PY_FIX)
	@printf '%s\n' "  if data[4]!=2 or data[5]!=1: print('[SKIP]',p); continue  # 64-bit LE" >> $(PY_FIX)
	@printf '%s\n' "  e_phoff=struct.unpack_from('<Q',data,0x20)[0]" >> $(PY_FIX)
	@printf '%s\n' "  e_phentsize=struct.unpack_from('<H',data,0x36)[0]" >> $(PY_FIX)
	@printf '%s\n' "  e_phnum=struct.unpack_from('<H',data,0x38)[0]" >> $(PY_FIX)
	@printf '%s\n' "  PT_GNU_STACK=0x6474E551; changed=False" >> $(PY_FIX)
	@printf '%s\n' "  for i in range(e_phnum):" >> $(PY_FIX)
	@printf '%s\n' "    off=e_phoff+i*e_phentsize" >> $(PY_FIX)
	@printf '%s\n' "    p_type=struct.unpack_from('<I',data,off+0)[0]" >> $(PY_FIX)
	@printf '%s\n' "    if p_type==PT_GNU_STACK:" >> $(PY_FIX)
	@printf '%s\n' "      flags=struct.unpack_from('<I',data,off+4)[0]" >> $(PY_FIX)
	@printf '%s\n' "      nflags=flags & ~0x1  # limpa PF_X" >> $(PY_FIX)
	@printf '%s\n' "      if nflags!=flags: struct.pack_into('<I',data,off+4,nflags); changed=True" >> $(PY_FIX)
	@printf '%s\n' "  if changed: open(p,'r+b').write(data); print('[OK ]',p,'EXEC removido')" >> $(PY_FIX)
	@printf '%s\n' "  else: print('[OK ]',p,'já estava sem EXEC')" >> $(PY_FIX)
	@chmod +x $(PY_FIX)

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

.PHONY: supers
supers: $(EXE_SUPERS) $(PY_FIX)
	@mkdir -p $(SUPERS_DIR)
	@set -e; \
	for f in $(TESTS); do \
	  base=$$(basename $$f .hsk); \
	  outdir=$(SUPERS_DIR)/$$base; \
	  depdir=$$outdir/ghc-deps; \
	  sofile=$$outdir/libsupers.so; \
	  mkdir -p $$outdir; \
	  echo "[SUPERS] $$f → $$outdir/Supers.hs"; \
	  GHC_ENVIRONMENT=- ./$(EXE_SUPERS) $$f > $$outdir/Supers.hs.tmp; \
	  if [ -s $$outdir/Supers.hs.tmp ]; then \
	    mv $$outdir/Supers.hs.tmp $$outdir/Supers.hs; \
	    echo "[GHC  ] $$sofile"; \
	    rtsbase="$(RTS_BASE_LIB)"; \
	    if [ -z "$$rtsbase" ]; then \
	      rtsbase=$$(basename $$(ls "$(GHC_RTS_DIR)"/libHSrts-ghc*.so | head -n1) .so | sed 's/^lib//'); \
	    fi; \
	    GHC_ENVIRONMENT=- $(GHC) $(SUPERS_LINK_FLAGS) \
	           -optl -L"$(GHC_RTS_DIR)" -optl -l$$rtsbase \
	           -o $$sofile $$outdir/Supers.hs; \
	    mkdir -p $$depdir; \
	    for pat in $(DEPS_PATTERNS); do \
	      for so in "$(GHC_LIBDIR)"/$$pat; do \
	        [ -f "$$so" ] && cp -L "$$so" "$$depdir/"; \
	      done; \
	    done; \
	    for g in $(GMP_CANDIDATES); do \
	      if [ -f "$$g" ]; then cp -L "$$g" "$$depdir/"; break; fi; \
	    done; \
	    if command -v patchelf >/dev/null 2>&1; then \
	      patchelf --force-rpath --set-rpath '$$ORIGIN/ghc-deps:$$ORIGIN' "$$sofile" || true; \
	    fi; \
	    echo "[STACK] removendo EXEC de $$sofile e deps (via Python)"; \
	    python3 "$(PY_FIX)" "$$sofile" "$$depdir"/*.so || true; \
	  else \
	    rm -f $$outdir/Supers.hs.tmp $$outdir/Supers.hs $$sofile; \
	    rm -rf $$depdir; \
	  fi; \
	done

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
	@rm -rf $(DF_OUT_DIR) $(AST_OUT_DIR) $(CODE_OUT_DIR) $(SUPERS_DIR)
