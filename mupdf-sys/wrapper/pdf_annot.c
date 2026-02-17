#include "internal.h"

int mupdf_pdf_annot_type(fz_context *ctx, pdf_annot *annot, mupdf_error_t **errptr)
{
    TRY_CATCH(int, 0, pdf_annot_type(ctx, annot));
}

const char *mupdf_pdf_annot_author(fz_context *ctx, pdf_annot *annot, mupdf_error_t **errptr)
{
    TRY_CATCH(const char *, NULL, pdf_annot_author(ctx, annot));
}

void mupdf_pdf_set_annot_author(fz_context *ctx, pdf_annot *annot, const char *author, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_set_annot_author(ctx, annot, author));
}

void mupdf_pdf_annot_line(fz_context *ctx, pdf_annot *annot, fz_point *a, fz_point *b, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_annot_line(ctx, annot, a, b));
}

void mupdf_pdf_set_annot_line(fz_context *ctx, pdf_annot *annot, fz_point a, fz_point b, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_set_annot_line(ctx, annot, a, b));
}

fz_rect mupdf_pdf_annot_rect(fz_context *ctx, pdf_annot *annot, mupdf_error_t **errptr)
{
    TRY_CATCH(fz_rect, fz_empty_rect, pdf_annot_rect(ctx, annot));
}

void mupdf_pdf_set_annot_rect(fz_context *ctx, pdf_annot *annot, fz_rect rect, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_set_annot_rect(ctx, annot, rect));
}

void mupdf_pdf_annot_color(fz_context *ctx, pdf_annot *annot, int *n, float *color, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_annot_color(ctx, annot, n, color));
}

void mupdf_pdf_set_annot_color(fz_context *ctx, pdf_annot *annot, int n, const float *color, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_set_annot_color(ctx, annot, n, color));
}

int mupdf_pdf_annot_flags(fz_context *ctx, pdf_annot *annot, mupdf_error_t **errptr)
{
    TRY_CATCH(int, 0, pdf_annot_flags(ctx, annot));
}

void mupdf_pdf_set_annot_flags(fz_context *ctx, pdf_annot *annot, int flags, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_set_annot_flags(ctx, annot, flags));
}

fz_rect mupdf_pdf_annot_popup(fz_context *ctx, pdf_annot *annot, mupdf_error_t **errptr)
{
    TRY_CATCH(fz_rect, fz_empty_rect, pdf_annot_popup(ctx, annot));
}

void mupdf_pdf_set_annot_popup(fz_context *ctx, pdf_annot *annot, fz_rect rect, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_set_annot_popup(ctx, annot, rect));
}

void mupdf_pdf_set_annot_active(fz_context *ctx, pdf_annot *annot, int active, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_set_annot_active(ctx, annot, active));
}

float mupdf_pdf_annot_border_width(fz_context *ctx, pdf_annot *annot, mupdf_error_t **errptr)
{
    TRY_CATCH(float, 0.0f, pdf_annot_border_width(ctx, annot));
}

void mupdf_pdf_set_annot_border_width(fz_context *ctx, pdf_annot *annot, float width, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_set_annot_border_width(ctx, annot, width));
}

enum pdf_intent mupdf_pdf_annot_intent(fz_context *ctx, pdf_annot *annot, mupdf_error_t **errptr)
{
    TRY_CATCH(enum pdf_intent, PDF_ANNOT_IT_DEFAULT, pdf_annot_intent(ctx, annot));
}

void mupdf_pdf_set_annot_intent(fz_context *ctx, pdf_annot *annot, enum pdf_intent intent, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_set_annot_intent(ctx, annot, intent));
}

void mupdf_pdf_filter_annot_contents(fz_context *ctx, pdf_annot *annot, pdf_filter_options *filter, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_filter_annot_contents(ctx, pdf_annot_page(ctx, annot)->doc, annot, filter));
}

pdf_obj *mupdf_pdf_annot_obj(fz_context *ctx, pdf_annot *annot, mupdf_error_t **errptr)
{
    TRY_CATCH(pdf_obj*, NULL, pdf_annot_obj(ctx, annot));
}

int mupdf_pdf_lookup_page_number(fz_context *ctx, pdf_document *doc, pdf_obj *page_obj, mupdf_error_t **errptr)
{
    TRY_CATCH(int, -1, pdf_lookup_page_number(ctx, doc, page_obj));
}
