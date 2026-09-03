/* LaTeX footnote extension for litedown.
 *
 * cmark's footnote support (CMARK_OPT_FOOTNOTES) parses `[^id]` references and
 * `[^id]: ...` definitions, then moves every definition to the end of the
 * document (after the last block). That layout is right for HTML (a
 * <section class="footnotes"> list at the bottom), but wrong for LaTeX: emitting
 * the note bodies at the end of the document would put all footnotes on the last
 * page instead of at the bottom of the page where they are cited.
 *
 * The LaTeX convention is `\footnote{body}` placed inline at the citation. This
 * extension rewrites the parsed footnote tree for LaTeX output so that:
 *   - the first reference to a footnote renders as `\footnote{body}`, with the
 *     definition's block content moved inline under the reference node;
 *   - any further reference to the same footnote renders as `\footnotemark[N]`
 *     (N is the footnote number cmark already stored in the reference literal),
 *     which reuses the number without advancing LaTeX's footnote counter;
 *   - the now-empty definitions left at the end of the document render nothing.
 *
 * This replaces two older mechanisms: a core `latex.c` patch that emitted
 * `\footnotemark[..]` / `\footnotetext[..]{..}`, and R's fix_footnotes() which
 * stitched those back together with regular expressions (and only handled the
 * single-reference case correctly). Doing it here keeps the transformation on
 * the parse tree, where reference counts and definition bodies are exact.
 *
 * Only latex_render_func is set, so every other output format keeps cmark's
 * built-in footnote rendering. The extension is attached only for LaTeX output
 * (see litedown_attach_extension); it does not create a new node type.
 *
 * This is a litedown-only file; it is not part of upstream cmark-gfm and is not
 * touched by src/patches/sync.sh.
 */

#include "latexfootnotes.h"
#include <parser.h>
#include <node.h>
#include <render.h>

#define LIT(s) renderer->out(renderer, node, s, false, LITERAL)

/* Move all block children of the footnote definition `def` to become the
 * children of the reference node `ref` (which is otherwise an inline leaf). The
 * built-in child API refuses block-into-inline moves (S_can_contain), so splice
 * the child list by pointer. The definition is left child-less; it renders
 * nothing when the iterator reaches it at the end of the document. */
static void adopt_definition_body(cmark_node *ref, cmark_node *def) {
  cmark_node *c;
  ref->first_child = def->first_child;
  ref->last_child = def->last_child;
  for (c = ref->first_child; c; c = c->next)
    c->parent = ref;
  def->first_child = NULL;
  def->last_child = NULL;
}

static void latex_render(cmark_syntax_extension *extension,
                         cmark_renderer *renderer, cmark_node *node,
                         cmark_event_type ev_type, int options) {
  bool entering = (ev_type == CMARK_EVENT_ENTER);
  (void)extension;
  (void)options;

  switch (node->type) {
  case CMARK_NODE_FOOTNOTE_REFERENCE:
    if (node->footnote.ref_ix > 1) {
      /* a repeated reference: reuse the number without moving the counter.
       * node->as.literal holds the footnote number cmark assigned. */
      if (entering) {
        LIT("\\footnotemark[");
        renderer->out(renderer, node,
                      cmark_chunk_to_cstr(renderer->mem, &node->as.literal),
                      false, LITERAL);
        LIT("]");
      }
      /* a leaf: no children to descend into */
    } else {
      /* the first reference carries the definition body (moved in during
       * postprocess); wrap it in \footnote{...} */
      if (entering) {
        LIT("\\footnote{");
      } else {
        /* drop the blank line the trailing paragraph requested so the closing
         * brace hugs the body: \footnote{body} rather than \footnote{body\n\n} */
        renderer->need_cr = 0;
        LIT("}");
      }
    }
    break;

  case CMARK_NODE_FOOTNOTE_DEFINITION:
    /* emptied in postprocess (its body moved to the first reference); render
     * nothing and skip whatever, if anything, remains under it */
    break;

  default:
    break;
  }
}

/* Reparent each footnote definition's body under its first reference, so the
 * LaTeX renderer can emit it inline as \footnote{body}. Tag every footnote
 * reference and definition with this extension so the render dispatch routes
 * them to latex_render above. */
static cmark_node *postprocess(cmark_syntax_extension *self, cmark_parser *parser,
                               cmark_node *root) {
  cmark_iter *iter = cmark_iter_new(root);
  cmark_event_type ev;
  (void)parser;

  while ((ev = cmark_iter_next(iter)) != CMARK_EVENT_DONE) {
    cmark_node *node = cmark_iter_get_node(iter);
    if (ev != CMARK_EVENT_ENTER)
      continue;
    if (node->type == CMARK_NODE_FOOTNOTE_REFERENCE) {
      cmark_node_set_syntax_extension(node, self);
      if (node->footnote.ref_ix <= 1 && node->parent_footnote_def)
        adopt_definition_body(node, node->parent_footnote_def);
    } else if (node->type == CMARK_NODE_FOOTNOTE_DEFINITION) {
      cmark_node_set_syntax_extension(node, self);
    }
  }

  cmark_iter_free(iter);
  return root;
}

cmark_syntax_extension *create_latexfootnotes_extension(void) {
  cmark_syntax_extension *ext = cmark_syntax_extension_new("latexfootnotes");

  cmark_syntax_extension_set_latex_render_func(ext, latex_render);
  cmark_syntax_extension_set_postprocess_func(ext, postprocess);

  return ext;
}
