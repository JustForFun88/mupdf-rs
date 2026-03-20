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

void mupdf_pdf_set_annot_line(fz_context *ctx, pdf_annot *annot, fz_point a, fz_point b, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_set_annot_line(ctx, annot, a, b));
}

void mupdf_pdf_set_annot_rect(fz_context *ctx, pdf_annot *annot, fz_rect rect, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_set_annot_rect(ctx, annot, rect));
}

void mupdf_pdf_set_annot_color(fz_context *ctx, pdf_annot *annot, int n, const float *color, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_set_annot_color(ctx, annot, n, color));
}

void mupdf_pdf_set_annot_flags(fz_context *ctx, pdf_annot *annot, int flags, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_set_annot_flags(ctx, annot, flags));
}

void mupdf_pdf_set_annot_popup(fz_context *ctx, pdf_annot *annot, fz_rect rect, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_set_annot_popup(ctx, annot, rect));
}

void mupdf_pdf_set_annot_active(fz_context *ctx, pdf_annot *annot, int active, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_set_annot_active(ctx, annot, active));
}

void mupdf_pdf_set_annot_border_width(fz_context *ctx, pdf_annot *annot, float width, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_set_annot_border_width(ctx, annot, width));
}

void mupdf_pdf_set_annot_intent(fz_context *ctx, pdf_annot *annot, enum pdf_intent intent, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_set_annot_intent(ctx, annot, intent));
}

void mupdf_pdf_filter_annot_contents(fz_context *ctx, pdf_annot *annot, pdf_filter_options *filter, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_filter_annot_contents(ctx, pdf_annot_page(ctx, annot)->doc, annot, filter));
}

fz_rect mupdf_pdf_annot_rect(fz_context *ctx, pdf_annot *annot, mupdf_error_t **errptr)
{
    TRY_CATCH(fz_rect, fz_make_rect(0, 0, 0, 0), pdf_annot_rect(ctx, annot));
}

void mupdf_pdf_annot_color(fz_context *ctx, pdf_annot *annot, int *n, float color[4], mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_annot_color(ctx, annot, n, color));
}

int mupdf_pdf_annot_flags(fz_context *ctx, pdf_annot *annot, mupdf_error_t **errptr)
{
    TRY_CATCH(int, 0, pdf_annot_flags(ctx, annot));
}

float mupdf_pdf_annot_border_width(fz_context *ctx, pdf_annot *annot, mupdf_error_t **errptr)
{
    TRY_CATCH(float, 0.0f, pdf_annot_border_width(ctx, annot));
}

void mupdf_pdf_annot_line(fz_context *ctx, pdf_annot *annot, fz_point *a, fz_point *b, mupdf_error_t **errptr)
{
    TRY_CATCH_VOID(pdf_annot_line(ctx, annot, a, b));
}

fz_rect mupdf_pdf_annot_popup(fz_context *ctx, pdf_annot *annot, mupdf_error_t **errptr)
{
    TRY_CATCH(fz_rect, fz_make_rect(0, 0, 0, 0), pdf_annot_popup(ctx, annot));
}

int mupdf_pdf_annot_intent(fz_context *ctx, pdf_annot *annot, mupdf_error_t **errptr)
{
    TRY_CATCH(int, 0, pdf_annot_intent(ctx, annot));
}

int mupdf_pdf_obj_annot_type(fz_context *ctx, pdf_obj *obj, mupdf_error_t **errptr)
{
    int result = PDF_ANNOT_UNKNOWN;
    fz_try(ctx)
    {
        pdf_obj *subtype = pdf_dict_get(ctx, obj, PDF_NAME(Subtype));
        const char *name = pdf_to_name(ctx, subtype);
        result = pdf_annot_type_from_string(ctx, name);
    }
    fz_catch(ctx)
    {
        mupdf_save_error(ctx, errptr);
    }
    return result;
}

pdf_obj *mupdf_pdf_annot_obj(fz_context *ctx, pdf_annot *annot, mupdf_error_t **errptr)
{
    pdf_obj *obj = NULL;
    fz_try(ctx)
    {
        obj = pdf_annot_obj(ctx, annot);
        pdf_keep_obj(ctx, obj);
    }
    fz_catch(ctx)
    {
        mupdf_save_error(ctx, errptr);
    }
    return obj;
}

int mupdf_pdf_lookup_page_number(fz_context *ctx, pdf_document *doc, pdf_obj *page_obj, mupdf_error_t **errptr)
{
    TRY_CATCH(int, -1, pdf_lookup_page_number(ctx, doc, page_obj));
}