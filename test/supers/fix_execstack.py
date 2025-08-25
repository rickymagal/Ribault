#!/usr/bin/env python3
# remove EXEC do PT_GNU_STACK em ELF64 little-endian
import sys,struct
if len(sys.argv)<2:print('uso: fix_execstack.py <lib.so>...');sys.exit(1)
for p in sys.argv[1:]:
  with open(p,'r+b') as f: data=bytearray(f.read())
  if data[:4]!=b'\x7fELF': print('[SKIP]',p); continue
  if data[4]!=2 or data[5]!=1: print('[SKIP]',p); continue  # 64-bit LE
  e_phoff=struct.unpack_from('<Q',data,0x20)[0]
  e_phentsize=struct.unpack_from('<H',data,0x36)[0]
  e_phnum=struct.unpack_from('<H',data,0x38)[0]
  PT_GNU_STACK=0x6474E551; changed=False
  for i in range(e_phnum):
    off=e_phoff+i*e_phentsize
    p_type=struct.unpack_from('<I',data,off+0)[0]
    if p_type==PT_GNU_STACK:
      flags=struct.unpack_from('<I',data,off+4)[0]
      nflags=flags & ~0x1  # limpa PF_X
      if nflags!=flags: struct.pack_into('<I',data,off+4,nflags); changed=True
  if changed: open(p,'r+b').write(data); print('[OK ]',p,'EXEC removido')
  else: print('[OK ]',p,'já estava sem EXEC')
