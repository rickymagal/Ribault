// tools/supers_rts_init.c
#include <HsFFI.h>
#include <stddef.h>  // NULL

static int hs_is_inited = 0;

__attribute__((constructor))
static void hs_startup(void) {
    if (hs_is_inited) return;
    hs_is_inited = 1;

    static int argc = 1;
    static char *argv[] = { (char*)"libsupers", NULL };
    char **pargv = argv;                 // hs_init quer char ***

    hs_init(&argc, &pargv);              // inicializa o runtime do GHC
}

__attribute__((destructor))
static void hs_shutdown(void) {
    if (!hs_is_inited) return;
    hs_is_inited = 0;
    hs_exit();                           // finaliza ao descarregar o .so
}
