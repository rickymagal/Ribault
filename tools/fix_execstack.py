#!/usr/bin/env python3
import sys
import struct

PT_GNU_STACK = 0x6474E551
PF_X = 0x1  # bit EXEC

def fix(path: str) -> None:
    try:
        with open(path, 'r+b') as f:
            data = bytearray(f.read())
        # Só trata ELF64 little-endian
        if len(data) < 0x40 or data[:4] != b'\x7fELF' or data[4] != 2 or data[5] != 1:
            print('[SKIP]', path, '(não é ELF64 LE)')
            return

        e_phoff   = struct.unpack_from('<Q', data, 0x20)[0]
        e_phentsz = struct.unpack_from('<H', data, 0x36)[0]
        e_phnum   = struct.unpack_from('<H', data, 0x38)[0]

        changed = False
        for i in range(e_phnum):
            off = e_phoff + i * e_phentsz
            if off + 0x20 > len(data):
                break
            p_type = struct.unpack_from('<I', data, off + 0)[0]
            if p_type == PT_GNU_STACK:
                flags = struct.unpack_from('<I', data, off + 4)[0]
                if flags & PF_X:
                    struct.pack_into('<I', data, off + 4, flags & ~PF_X)
                    changed = True

        if changed:
            with open(path, 'r+b') as f:
                f.write(data)
            print('[OK ]', path, ': EXEC removido do PT_GNU_STACK')
        else:
            print('[OK ]', path, ': já estava sem EXEC no PT_GNU_STACK')

    except Exception as e:
        print('[ERR]', path, e)

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('uso: fix_execstack.py <lib.so>...')
        sys.exit(1)
    for p in sys.argv[1:]:
        fix(p)
