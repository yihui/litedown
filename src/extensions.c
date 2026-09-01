#include <Rinternals.h>
#include "syntax_extension.h"
#include "registry.h"

SEXP R_list_extensions(void){
  cmark_mem *mem = cmark_get_default_mem_allocator();
  cmark_llist *syntax_extensions = cmark_list_syntax_extensions(mem);
  cmark_llist *tmp = syntax_extensions;
  int len;
  for(len = 0; tmp != NULL;len++){
    tmp = tmp->next;
  }
  SEXP out = PROTECT(Rf_allocVector(STRSXP, len));
  int i = 0;
  for (tmp = syntax_extensions; tmp; tmp=tmp->next) {
    cmark_syntax_extension *ext = tmp->data;
    SET_STRING_ELT(out, i, Rf_mkChar(ext->name));
    i++;
  }
  cmark_llist_free(mem, syntax_extensions);
  UNPROTECT(1);
  return out;
}
