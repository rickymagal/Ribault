#include <stdint.h>
#include <stdio.h>
#include "queue.h"
#include "interp.h"

#if defined(__GNUC__)
#  define EXPORT_FN __attribute__((visibility("default")))
#  define WEAK_FN   __attribute__((weak))
#  define USED_FN   __attribute__((used))
#else
#  define EXPORT_FN
#  define WEAK_FN
#  define USED_FN
#endif

static int64_t supers_missing(int n) {
  fprintf(stderr, "[supers] missing symbol: s%d\n", n);
  return 0;
}

extern void s0(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super0(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s0) {
    s0(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(0);
  }
}

extern void s1(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super1(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s1) {
    s1(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(1);
  }
}

extern void s2(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super2(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s2) {
    s2(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(2);
  }
}

extern void s3(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super3(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s3) {
    s3(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(3);
  }
}

extern void s4(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super4(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s4) {
    s4(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(4);
  }
}

extern void s5(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super5(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s5) {
    s5(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(5);
  }
}

extern void s6(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super6(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s6) {
    s6(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(6);
  }
}

extern void s7(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super7(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s7) {
    s7(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(7);
  }
}

extern void s8(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super8(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s8) {
    s8(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(8);
  }
}

extern void s9(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super9(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s9) {
    s9(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(9);
  }
}

extern void s10(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super10(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s10) {
    s10(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(10);
  }
}

extern void s11(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super11(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s11) {
    s11(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(11);
  }
}

extern void s12(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super12(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s12) {
    s12(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(12);
  }
}

extern void s13(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super13(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s13) {
    s13(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(13);
  }
}

extern void s14(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super14(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s14) {
    s14(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(14);
  }
}

extern void s15(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super15(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s15) {
    s15(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(15);
  }
}

extern void s16(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super16(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s16) {
    s16(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(16);
  }
}

extern void s17(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super17(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s17) {
    s17(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(17);
  }
}

extern void s18(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super18(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s18) {
    s18(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(18);
  }
}

extern void s19(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super19(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s19) {
    s19(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(19);
  }
}

extern void s20(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super20(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s20) {
    s20(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(20);
  }
}

extern void s21(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super21(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s21) {
    s21(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(21);
  }
}

extern void s22(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super22(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s22) {
    s22(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(22);
  }
}

extern void s23(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super23(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s23) {
    s23(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(23);
  }
}

extern void s24(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super24(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s24) {
    s24(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(24);
  }
}

extern void s25(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super25(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s25) {
    s25(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(25);
  }
}

extern void s26(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super26(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s26) {
    s26(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(26);
  }
}

extern void s27(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super27(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s27) {
    s27(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(27);
  }
}

extern void s28(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super28(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s28) {
    s28(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(28);
  }
}

extern void s29(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super29(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s29) {
    s29(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(29);
  }
}

extern void s30(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super30(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s30) {
    s30(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(30);
  }
}

extern void s31(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super31(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s31) {
    s31(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(31);
  }
}

extern void s32(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super32(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s32) {
    s32(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(32);
  }
}

extern void s33(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super33(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s33) {
    s33(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(33);
  }
}

extern void s34(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super34(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s34) {
    s34(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(34);
  }
}

extern void s35(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super35(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s35) {
    s35(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(35);
  }
}

extern void s36(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super36(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s36) {
    s36(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(36);
  }
}

extern void s37(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super37(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s37) {
    s37(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(37);
  }
}

extern void s38(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super38(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s38) {
    s38(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(38);
  }
}

extern void s39(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super39(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s39) {
    s39(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(39);
  }
}

extern void s40(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super40(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s40) {
    s40(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(40);
  }
}

extern void s41(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super41(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s41) {
    s41(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(41);
  }
}

extern void s42(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super42(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s42) {
    s42(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(42);
  }
}

extern void s43(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super43(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s43) {
    s43(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(43);
  }
}

extern void s44(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super44(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s44) {
    s44(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(44);
  }
}

extern void s45(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super45(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s45) {
    s45(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(45);
  }
}

extern void s46(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super46(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s46) {
    s46(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(46);
  }
}

extern void s47(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super47(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s47) {
    s47(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(47);
  }
}

extern void s48(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super48(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s48) {
    s48(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(48);
  }
}

extern void s49(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super49(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s49) {
    s49(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(49);
  }
}

extern void s50(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super50(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s50) {
    s50(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(50);
  }
}

extern void s51(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super51(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s51) {
    s51(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(51);
  }
}

extern void s52(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super52(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s52) {
    s52(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(52);
  }
}

extern void s53(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super53(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s53) {
    s53(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(53);
  }
}

extern void s54(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super54(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s54) {
    s54(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(54);
  }
}

extern void s55(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super55(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s55) {
    s55(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(55);
  }
}

extern void s56(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super56(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s56) {
    s56(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(56);
  }
}

extern void s57(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super57(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s57) {
    s57(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(57);
  }
}

extern void s58(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super58(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s58) {
    s58(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(58);
  }
}

extern void s59(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super59(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s59) {
    s59(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(59);
  }
}

extern void s60(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super60(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s60) {
    s60(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(60);
  }
}

extern void s61(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super61(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s61) {
    s61(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(61);
  }
}

extern void s62(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super62(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s62) {
    s62(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(62);
  }
}

extern void s63(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super63(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s63) {
    s63(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(63);
  }
}

extern void s64(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super64(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s64) {
    s64(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(64);
  }
}

extern void s65(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super65(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s65) {
    s65(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(65);
  }
}

extern void s66(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super66(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s66) {
    s66(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(66);
  }
}

extern void s67(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super67(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s67) {
    s67(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(67);
  }
}

extern void s68(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super68(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s68) {
    s68(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(68);
  }
}

extern void s69(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super69(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s69) {
    s69(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(69);
  }
}

extern void s70(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super70(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s70) {
    s70(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(70);
  }
}

extern void s71(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super71(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s71) {
    s71(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(71);
  }
}

extern void s72(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super72(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s72) {
    s72(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(72);
  }
}

extern void s73(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super73(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s73) {
    s73(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(73);
  }
}

extern void s74(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super74(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s74) {
    s74(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(74);
  }
}

extern void s75(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super75(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s75) {
    s75(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(75);
  }
}

extern void s76(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super76(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s76) {
    s76(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(76);
  }
}

extern void s77(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super77(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s77) {
    s77(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(77);
  }
}

extern void s78(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super78(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s78) {
    s78(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(78);
  }
}

extern void s79(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super79(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s79) {
    s79(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(79);
  }
}

extern void s80(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super80(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s80) {
    s80(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(80);
  }
}

extern void s81(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super81(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s81) {
    s81(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(81);
  }
}

extern void s82(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super82(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s82) {
    s82(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(82);
  }
}

extern void s83(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super83(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s83) {
    s83(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(83);
  }
}

extern void s84(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super84(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s84) {
    s84(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(84);
  }
}

extern void s85(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super85(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s85) {
    s85(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(85);
  }
}

extern void s86(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super86(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s86) {
    s86(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(86);
  }
}

extern void s87(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super87(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s87) {
    s87(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(87);
  }
}

extern void s88(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super88(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s88) {
    s88(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(88);
  }
}

extern void s89(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super89(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s89) {
    s89(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(89);
  }
}

extern void s90(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super90(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s90) {
    s90(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(90);
  }
}

extern void s91(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super91(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s91) {
    s91(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(91);
  }
}

extern void s92(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super92(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s92) {
    s92(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(92);
  }
}

extern void s93(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super93(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s93) {
    s93(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(93);
  }
}

extern void s94(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super94(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s94) {
    s94(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(94);
  }
}

extern void s95(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super95(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s95) {
    s95(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(95);
  }
}

extern void s96(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super96(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s96) {
    s96(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(96);
  }
}

extern void s97(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super97(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s97) {
    s97(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(97);
  }
}

extern void s98(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super98(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s98) {
    s98(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(98);
  }
}

extern void s99(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super99(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s99) {
    s99(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(99);
  }
}

extern void s100(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super100(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s100) {
    s100(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(100);
  }
}

extern void s101(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super101(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s101) {
    s101(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(101);
  }
}

extern void s102(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super102(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s102) {
    s102(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(102);
  }
}

extern void s103(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super103(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s103) {
    s103(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(103);
  }
}

extern void s104(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super104(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s104) {
    s104(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(104);
  }
}

extern void s105(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super105(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s105) {
    s105(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(105);
  }
}

extern void s106(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super106(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s106) {
    s106(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(106);
  }
}

extern void s107(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super107(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s107) {
    s107(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(107);
  }
}

extern void s108(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super108(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s108) {
    s108(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(108);
  }
}

extern void s109(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super109(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s109) {
    s109(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(109);
  }
}

extern void s110(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super110(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s110) {
    s110(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(110);
  }
}

extern void s111(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super111(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s111) {
    s111(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(111);
  }
}

extern void s112(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super112(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s112) {
    s112(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(112);
  }
}

extern void s113(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super113(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s113) {
    s113(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(113);
  }
}

extern void s114(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super114(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s114) {
    s114(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(114);
  }
}

extern void s115(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super115(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s115) {
    s115(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(115);
  }
}

extern void s116(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super116(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s116) {
    s116(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(116);
  }
}

extern void s117(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super117(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s117) {
    s117(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(117);
  }
}

extern void s118(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super118(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s118) {
    s118(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(118);
  }
}

extern void s119(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super119(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s119) {
    s119(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(119);
  }
}

extern void s120(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super120(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s120) {
    s120(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(120);
  }
}

extern void s121(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super121(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s121) {
    s121(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(121);
  }
}

extern void s122(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super122(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s122) {
    s122(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(122);
  }
}

extern void s123(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super123(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s123) {
    s123(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(123);
  }
}

extern void s124(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super124(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s124) {
    s124(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(124);
  }
}

extern void s125(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super125(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s125) {
    s125(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(125);
  }
}

extern void s126(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super126(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s126) {
    s126(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(126);
  }
}

extern void s127(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super127(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s127) {
    s127(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(127);
  }
}

extern void s128(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super128(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s128) {
    s128(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(128);
  }
}

extern void s129(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super129(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s129) {
    s129(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(129);
  }
}

extern void s130(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super130(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s130) {
    s130(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(130);
  }
}

extern void s131(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super131(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s131) {
    s131(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(131);
  }
}

extern void s132(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super132(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s132) {
    s132(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(132);
  }
}

extern void s133(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super133(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s133) {
    s133(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(133);
  }
}

extern void s134(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super134(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s134) {
    s134(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(134);
  }
}

extern void s135(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super135(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s135) {
    s135(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(135);
  }
}

extern void s136(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super136(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s136) {
    s136(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(136);
  }
}

extern void s137(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super137(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s137) {
    s137(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(137);
  }
}

extern void s138(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super138(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s138) {
    s138(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(138);
  }
}

extern void s139(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super139(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s139) {
    s139(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(139);
  }
}

extern void s140(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super140(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s140) {
    s140(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(140);
  }
}

extern void s141(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super141(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s141) {
    s141(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(141);
  }
}

extern void s142(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super142(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s142) {
    s142(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(142);
  }
}

extern void s143(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super143(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s143) {
    s143(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(143);
  }
}

extern void s144(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super144(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s144) {
    s144(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(144);
  }
}

extern void s145(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super145(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s145) {
    s145(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(145);
  }
}

extern void s146(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super146(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s146) {
    s146(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(146);
  }
}

extern void s147(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super147(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s147) {
    s147(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(147);
  }
}

extern void s148(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super148(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s148) {
    s148(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(148);
  }
}

extern void s149(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super149(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s149) {
    s149(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(149);
  }
}

extern void s150(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super150(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s150) {
    s150(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(150);
  }
}

extern void s151(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super151(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s151) {
    s151(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(151);
  }
}

extern void s152(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super152(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s152) {
    s152(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(152);
  }
}

extern void s153(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super153(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s153) {
    s153(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(153);
  }
}

extern void s154(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super154(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s154) {
    s154(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(154);
  }
}

extern void s155(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super155(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s155) {
    s155(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(155);
  }
}

extern void s156(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super156(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s156) {
    s156(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(156);
  }
}

extern void s157(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super157(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s157) {
    s157(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(157);
  }
}

extern void s158(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super158(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s158) {
    s158(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(158);
  }
}

extern void s159(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super159(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s159) {
    s159(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(159);
  }
}

extern void s160(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super160(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s160) {
    s160(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(160);
  }
}

extern void s161(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super161(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s161) {
    s161(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(161);
  }
}

extern void s162(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super162(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s162) {
    s162(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(162);
  }
}

extern void s163(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super163(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s163) {
    s163(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(163);
  }
}

extern void s164(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super164(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s164) {
    s164(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(164);
  }
}

extern void s165(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super165(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s165) {
    s165(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(165);
  }
}

extern void s166(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super166(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s166) {
    s166(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(166);
  }
}

extern void s167(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super167(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s167) {
    s167(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(167);
  }
}

extern void s168(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super168(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s168) {
    s168(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(168);
  }
}

extern void s169(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super169(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s169) {
    s169(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(169);
  }
}

extern void s170(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super170(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s170) {
    s170(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(170);
  }
}

extern void s171(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super171(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s171) {
    s171(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(171);
  }
}

extern void s172(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super172(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s172) {
    s172(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(172);
  }
}

extern void s173(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super173(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s173) {
    s173(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(173);
  }
}

extern void s174(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super174(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s174) {
    s174(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(174);
  }
}

extern void s175(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super175(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s175) {
    s175(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(175);
  }
}

extern void s176(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super176(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s176) {
    s176(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(176);
  }
}

extern void s177(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super177(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s177) {
    s177(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(177);
  }
}

extern void s178(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super178(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s178) {
    s178(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(178);
  }
}

extern void s179(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super179(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s179) {
    s179(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(179);
  }
}

extern void s180(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super180(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s180) {
    s180(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(180);
  }
}

extern void s181(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super181(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s181) {
    s181(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(181);
  }
}

extern void s182(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super182(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s182) {
    s182(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(182);
  }
}

extern void s183(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super183(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s183) {
    s183(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(183);
  }
}

extern void s184(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super184(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s184) {
    s184(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(184);
  }
}

extern void s185(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super185(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s185) {
    s185(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(185);
  }
}

extern void s186(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super186(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s186) {
    s186(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(186);
  }
}

extern void s187(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super187(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s187) {
    s187(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(187);
  }
}

extern void s188(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super188(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s188) {
    s188(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(188);
  }
}

extern void s189(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super189(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s189) {
    s189(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(189);
  }
}

extern void s190(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super190(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s190) {
    s190(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(190);
  }
}

extern void s191(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super191(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s191) {
    s191(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(191);
  }
}

extern void s192(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super192(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s192) {
    s192(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(192);
  }
}

extern void s193(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super193(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s193) {
    s193(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(193);
  }
}

extern void s194(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super194(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s194) {
    s194(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(194);
  }
}

extern void s195(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super195(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s195) {
    s195(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(195);
  }
}

extern void s196(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super196(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s196) {
    s196(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(196);
  }
}

extern void s197(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super197(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s197) {
    s197(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(197);
  }
}

extern void s198(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super198(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s198) {
    s198(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(198);
  }
}

extern void s199(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super199(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s199) {
    s199(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(199);
  }
}

extern void s200(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super200(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s200) {
    s200(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(200);
  }
}

extern void s201(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super201(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s201) {
    s201(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(201);
  }
}

extern void s202(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super202(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s202) {
    s202(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(202);
  }
}

extern void s203(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super203(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s203) {
    s203(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(203);
  }
}

extern void s204(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super204(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s204) {
    s204(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(204);
  }
}

extern void s205(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super205(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s205) {
    s205(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(205);
  }
}

extern void s206(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super206(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s206) {
    s206(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(206);
  }
}

extern void s207(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super207(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s207) {
    s207(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(207);
  }
}

extern void s208(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super208(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s208) {
    s208(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(208);
  }
}

extern void s209(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super209(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s209) {
    s209(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(209);
  }
}

extern void s210(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super210(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s210) {
    s210(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(210);
  }
}

extern void s211(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super211(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s211) {
    s211(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(211);
  }
}

extern void s212(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super212(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s212) {
    s212(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(212);
  }
}

extern void s213(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super213(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s213) {
    s213(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(213);
  }
}

extern void s214(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super214(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s214) {
    s214(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(214);
  }
}

extern void s215(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super215(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s215) {
    s215(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(215);
  }
}

extern void s216(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super216(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s216) {
    s216(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(216);
  }
}

extern void s217(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super217(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s217) {
    s217(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(217);
  }
}

extern void s218(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super218(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s218) {
    s218(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(218);
  }
}

extern void s219(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super219(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s219) {
    s219(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(219);
  }
}

extern void s220(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super220(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s220) {
    s220(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(220);
  }
}

extern void s221(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super221(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s221) {
    s221(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(221);
  }
}

extern void s222(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super222(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s222) {
    s222(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(222);
  }
}

extern void s223(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super223(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s223) {
    s223(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(223);
  }
}

extern void s224(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super224(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s224) {
    s224(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(224);
  }
}

extern void s225(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super225(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s225) {
    s225(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(225);
  }
}

extern void s226(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super226(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s226) {
    s226(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(226);
  }
}

extern void s227(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super227(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s227) {
    s227(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(227);
  }
}

extern void s228(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super228(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s228) {
    s228(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(228);
  }
}

extern void s229(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super229(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s229) {
    s229(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(229);
  }
}

extern void s230(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super230(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s230) {
    s230(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(230);
  }
}

extern void s231(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super231(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s231) {
    s231(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(231);
  }
}

extern void s232(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super232(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s232) {
    s232(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(232);
  }
}

extern void s233(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super233(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s233) {
    s233(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(233);
  }
}

extern void s234(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super234(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s234) {
    s234(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(234);
  }
}

extern void s235(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super235(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s235) {
    s235(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(235);
  }
}

extern void s236(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super236(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s236) {
    s236(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(236);
  }
}

extern void s237(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super237(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s237) {
    s237(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(237);
  }
}

extern void s238(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super238(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s238) {
    s238(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(238);
  }
}

extern void s239(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super239(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s239) {
    s239(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(239);
  }
}

extern void s240(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super240(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s240) {
    s240(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(240);
  }
}

extern void s241(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super241(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s241) {
    s241(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(241);
  }
}

extern void s242(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super242(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s242) {
    s242(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(242);
  }
}

extern void s243(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super243(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s243) {
    s243(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(243);
  }
}

extern void s244(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super244(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s244) {
    s244(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(244);
  }
}

extern void s245(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super245(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s245) {
    s245(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(245);
  }
}

extern void s246(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super246(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s246) {
    s246(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(246);
  }
}

extern void s247(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super247(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s247) {
    s247(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(247);
  }
}

extern void s248(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super248(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s248) {
    s248(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(248);
  }
}

extern void s249(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super249(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s249) {
    s249(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(249);
  }
}

extern void s250(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super250(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s250) {
    s250(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(250);
  }
}

extern void s251(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super251(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s251) {
    s251(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(251);
  }
}

extern void s252(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super252(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s252) {
    s252(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(252);
  }
}

extern void s253(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super253(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s253) {
    s253(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(253);
  }
}

extern void s254(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super254(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s254) {
    s254(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(254);
  }
}

extern void s255(int64_t *in, int64_t *out) WEAK_FN;
EXPORT_FN USED_FN void super255(oper_t **oper, oper_t *result) {
  int64_t in[2];
  int64_t out[1];
  in[0] = (int64_t)oper[0]->value.li;
  in[1] = 0;
  if (oper[1] != NULL) { in[1] = (int64_t)oper[1]->value.li; }
  if (s255) {
    s255(in, out);
    result[0].value.li = out[0];
  } else {
    result[0].value.li = supers_missing(255);
  }
}

