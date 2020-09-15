#include <stdio.h>
#include <string.h>
#include <png.h>

#include <Rinternals.h>
/* for R_RGB / R_RGBA */
#include <R_ext/GraphicsEngine.h>

typedef struct read_job {
    FILE *f;
    int ptr, len;
    char *data;
} read_job_t;

static void user_error_fn(png_structp png_ptr, png_const_charp error_msg) {
    read_job_t *rj = (read_job_t*)png_get_error_ptr(png_ptr);
    if (rj->f) fclose(rj->f);
    Rf_error("libpng error: %s", error_msg);
}

static void user_warning_fn(png_structp png_ptr, png_const_charp warning_msg) {
    Rf_warning("libpng warning: %s", warning_msg);
}

static void user_read_data(png_structp png_ptr, png_bytep data, png_size_t length) {
    read_job_t *rj = (read_job_t*) png_get_io_ptr(png_ptr);
    png_size_t to_read = length;
    if (to_read > (rj->len - rj->ptr))
	to_read = (rj->len - rj->ptr);
    if (to_read > 0) {
	memcpy(data, rj->data + rj->ptr, to_read);
	rj->ptr += to_read;
    }
    if (to_read < length)
	memset(data + length - to_read, 0, length - to_read);
}

#if USE_R_MALLOC
static png_voidp malloc_fn(png_structp png_ptr, png_alloc_size_t size) {
    return (png_voidp) R_alloc(1, size);
}

static void free_fn(png_structp png_ptr, png_voidp ptr) {
    /* this is a no-op because R releases the memory at the end of the call */
}
#endif

#define RX_swap32(X) (X) = (((unsigned int)X) >> 24) | ((((unsigned int)X) >> 8) & 0xff00) | (((unsigned int)X) << 24) | ((((unsigned int)X) & 0xff00) << 8)

SEXP read_png(SEXP sFn, SEXP sNative, SEXP sInfo) {
    SEXP res = R_NilValue, info_list = R_NilValue, info_tail = R_NilValue;
    const char *fn;
    char header[8];
    int native = asInteger(sNative), info = (asInteger(sInfo) == 1);
    FILE *f;
    read_job_t rj;
    png_structp png_ptr;
    png_infop info_ptr;
    
    if (TYPEOF(sFn) == RAWSXP) {
	rj.data = (char*) RAW(sFn);
	rj.len = LENGTH(sFn);
	rj.ptr = 0;
	rj.f = f = 0;
    } else {
	if (TYPEOF(sFn) != STRSXP || LENGTH(sFn) < 1) Rf_error("invalid filename");
	fn = CHAR(STRING_ELT(sFn, 0));
	f = fopen(fn, "rb");
	if (!f) Rf_error("unable to open %s", fn);
	if (fread(header, 1, 8, f) < 1 || png_sig_cmp((png_bytep) header, 0, 8)) {
	    fclose(f);
	    Rf_error("file is not in PNG format");
	}
	rj.f = f;
    }

    /* use our own error hanlding code and pass the fp so it can be closed on error */
    png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, (png_voidp)&rj, user_error_fn, user_warning_fn);
    if (!png_ptr) {
	if (f) fclose(f);
	Rf_error("unable to initialize libpng");
    }
    
    info_ptr = png_create_info_struct(png_ptr);
    if (!info_ptr) {
	if (f) fclose(f);
	png_destroy_read_struct(&png_ptr, (png_infopp)NULL, (png_infopp)NULL);
	Rf_error("unable to initialize libpng");
    }
    
    if (f) {
	png_init_io(png_ptr, f);
	png_set_sig_bytes(png_ptr, 8);
    } else
	png_set_read_fn(png_ptr, (png_voidp) &rj, user_read_data);

#define add_info(K, V) { info_tail = SETCDR(info_tail, CONS(V, R_NilValue)); SET_TAG(info_tail, install(K)); }

    /* png_read_png(png_ptr, info_ptr, PNG_TRANSFORM_STRIP_16 | PNG_TRANSFORM_EXPAND, NULL); */
    png_read_info(png_ptr, info_ptr);
    {
	png_uint_32 width, height;
	png_bytepp row_pointers;
	char *img_memory;
	SEXP dim;
	int bit_depth, color_type, interlace_type, compression_type, filter_method, rowbytes;
	int need_swap = 0;
	png_get_IHDR(png_ptr, info_ptr, &width, &height,
		     &bit_depth, &color_type, &interlace_type,
		     &compression_type, &filter_method);
	rowbytes = png_get_rowbytes(png_ptr, info_ptr);
#if VERBOSE_INFO
	Rprintf("png: %d x %d [%d], %d bytes, 0x%x, %d, %d\n", (int) width, (int) height, bit_depth, rowbytes,
		color_type, interlace_type, compression_type, filter_method);
#endif

	if (info) {
	    SEXP dv;
	    double d;
	    png_uint_32 rx, ry;
	    int ut, num_text = 0;
	    png_textp text_ptr;

	    info_tail = info_list = PROTECT(CONS((dv = allocVector(INTSXP, 2)), R_NilValue));
	    INTEGER(dv)[0] = (int) width;
	    INTEGER(dv)[1] = (int) height;
	    SET_TAG(info_list, install("dim"));
	    add_info("bit.depth", ScalarInteger(bit_depth));
	    switch(color_type) {
	    case PNG_COLOR_TYPE_GRAY: add_info("color.type", mkString("gray")); break;
	    case PNG_COLOR_TYPE_GRAY_ALPHA: add_info("color.type", mkString("gray + alpha")); break;
	    case PNG_COLOR_TYPE_PALETTE: add_info("color.type", mkString("palette")); break;
	    case PNG_COLOR_TYPE_RGB: add_info("color.type", mkString("RGB")); break;
	    case PNG_COLOR_TYPE_RGB_ALPHA: add_info("color.type", mkString("RGBA")); break;
	    default: add_info("color.type", ScalarInteger(color_type));
	    }
	    if (png_get_gAMA(png_ptr, info_ptr, &d)) add_info("gamma", ScalarReal(d));
#ifdef PNG_pHYs_SUPPORTED
	    if (png_get_pHYs(png_ptr, info_ptr, &rx, &ry, &ut)) {
		if (ut == PNG_RESOLUTION_METER) {
		    dv = allocVector(REALSXP, 2);
		    REAL(dv)[0] = ((double)rx) / 39.37008;
		    REAL(dv)[1] = ((double)ry) / 39.37008;
		    add_info("dpi", dv);
		} else if (ut == PNG_RESOLUTION_UNKNOWN)
		    add_info("asp", ScalarReal(rx / ry));
	    }
	    if (png_get_text(png_ptr, info_ptr, &text_ptr, &num_text)) {
		SEXP txt_key, txt_val = PROTECT(allocVector(STRSXP, num_text));
		if (num_text) {
		    int i;
		    setAttrib(txt_val, R_NamesSymbol, txt_key = allocVector(STRSXP, num_text));
		    for (i = 0; i < num_text; i++) {
			SET_STRING_ELT(txt_val, i, text_ptr[i].text ? mkChar(text_ptr[i].text) : NA_STRING);
			SET_STRING_ELT(txt_key, i, text_ptr[i].key ? mkChar(text_ptr[i].key) : NA_STRING);
		    }
		}
		add_info("text", txt_val);
		UNPROTECT(1);
	    }
#endif
	}

	/* on little-endian machines it's all well, but on big-endian ones we'll have to swap */
#if ! defined (__BIG_ENDIAN__) && ! defined (__LITTLE_ENDIAN__)   /* old compiler so have to use run-time check */
	{
	    char bo[4] = { 1, 0, 0, 0 };
	    int bi;
	    memcpy(&bi, bo, 4);
	    if (bi != 1)
		need_swap = 1;
	}
#endif
#ifdef __BIG_ENDIAN__
	need_swap = 1;
#endif

	/*==== set any transforms that we desire: ====*/
	/* palette->RGB - no discussion there */
	if (color_type == PNG_COLOR_TYPE_PALETTE)
	    png_set_palette_to_rgb(png_ptr);
	/* expand gray scale to 8 bits */
	if (color_type == PNG_COLOR_TYPE_GRAY &&
	    bit_depth < 8) png_set_expand_gray_1_2_4_to_8(png_ptr);
	/* this should not be necessary but it's in the docs to guarantee 8-bit */
	if (bit_depth < 8)
	    png_set_packing(png_ptr);
	/* convert tRNS chunk into alpha */
	if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS))
	    png_set_tRNS_to_alpha(png_ptr);
	/* native format doesn't allow for 16-bit so it needs to be truncated */
	if (bit_depth == 16 && native) {
	    Rf_warning("Image uses 16-bit channels but R native format only supports 8-bit, truncating LSB."); 
	    png_set_strip_16(png_ptr);
	}
	/* for native output we need to a) convert gray to RGB, b) add alpha */
	if (native) {
	    if (color_type == PNG_COLOR_TYPE_GRAY || color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
		png_set_gray_to_rgb(png_ptr);
	    if (!(color_type & PNG_COLOR_MASK_ALPHA)) /* if there is no alpha, add it */
		png_set_add_alpha(png_ptr, 0xFF, PNG_FILLER_AFTER);
	}
#if 0 /* we use native (network) endianness since we read each byte anyway */
	/* on little-endian machines we need to swap 16-bit values - this is the inverse of need_swap as used for R! */
	if (!need_swap && bit_depth == 16)
	    png_set_swap(png_ptr);
#endif

	/* PNG wants up to call png_set_interlace_handling so it can get ready to de-interlace images */
	png_set_interlace_handling(png_ptr);

	/* all transformations are in place, so it's time to update the info structure so we can allocate stuff */
	png_read_update_info(png_ptr, info_ptr);

	/* re-read some important bits from the updated structure */
	rowbytes = png_get_rowbytes(png_ptr, info_ptr);
	bit_depth = png_get_bit_depth(png_ptr, info_ptr);
	color_type = png_get_color_type(png_ptr, info_ptr);

#if VERBOSE_INFO
	Rprintf("   -filter-> %d-bits, %d bytes, 0x%x\n", bit_depth, rowbytes, color_type);
#endif

	/* allocate data fro row pointers and the image using R's allocation */
	row_pointers = (png_bytepp) R_alloc(height, sizeof(png_bytep));
	img_memory = R_alloc(height, rowbytes);
	{ /* populate the row pointers */
	    char *i_ptr = img_memory;
	    int i;
	    for (i = 0; i < height; i++, i_ptr += rowbytes)
	      row_pointers[i] = (png_bytep) i_ptr;
	}
	
	/* do the reading work */
	png_read_image(png_ptr, row_pointers);
	
	if (f) {
	    rj.f = 0;
	    fclose(f);
	}

	/* native output - vector of integers */
	if (native) {
	    int pln = rowbytes / width;
	    if (pln < 1 || pln > 4) {
		png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);
		Rf_error("native output for %d planes is not possible.", pln);
	    }

	    res = PROTECT(allocVector(INTSXP, width * height));
	    if (pln == 4) { /* 4 planes - efficient - just copy it all */
		int y, *idata = INTEGER(res);
		for (y = 0; y < height; idata += width, y++)
		    memcpy(idata, row_pointers[y], width * sizeof(int));

		if (need_swap) {
		    int *ide = idata;
		    idata = INTEGER(res);
		    for (; idata < ide; idata++)
			RX_swap32(*idata);
		}
	    } else if (pln == 3) { /* RGB */
		int x, y, *idata = INTEGER(res);
		for (y = 0; y < height; y++)
		    for (x = 0; x < rowbytes; x += 3)
			*(idata++) = R_RGB((unsigned int) row_pointers[y][x],
					   (unsigned int) row_pointers[y][x + 1],
					   (unsigned int) row_pointers[y][x + 2]);
	    } else if (pln == 2) { /* GA */
		int x, y, *idata = INTEGER(res);
		for (y = 0; y < height; y++)
		    for (x = 0; x < rowbytes; x += 2)
			*(idata++) = R_RGBA((unsigned int) row_pointers[y][x],
					    (unsigned int) row_pointers[y][x],
					    (unsigned int) row_pointers[y][x],
					    (unsigned int) row_pointers[y][x + 1]);
	    } else { /* gray */
		int x, y, *idata = INTEGER(res);
		for (y = 0; y < height; y++)
		    for (x = 0; x < rowbytes; x++)
			*(idata++) = R_RGB((unsigned int) row_pointers[y][x],
					   (unsigned int) row_pointers[y][x],
					   (unsigned int) row_pointers[y][x]);
	    }
	    dim = allocVector(INTSXP, 2);
	    INTEGER(dim)[0] = height;
	    INTEGER(dim)[1] = width;
	    setAttrib(res, R_DimSymbol, dim);
	    setAttrib(res, R_ClassSymbol, mkString("nativeRaster"));
	    setAttrib(res, install("channels"), ScalarInteger(pln));
	    UNPROTECT(1);
	} else {
	    int x, y, p, pln = rowbytes / width, pls = width * height;
	    double * data;
	    if (bit_depth == 16) {
		res = PROTECT(allocVector(REALSXP, (rowbytes * height) / 2));
		pln /= 2;
	    } else
		res = PROTECT(allocVector(REALSXP, rowbytes * height));

	    data = REAL(res);
	    if (bit_depth == 16)
		for(y = 0; y < height; y++)
		    for (x = 0; x < width; x++)
			for (p = 0; p < pln; p++)
			    data[y + x * height + p * pls] = ((double)(
								       (((unsigned int)(((unsigned char *)row_pointers[y])[2 * (x * pln + p)])) << 8) |
								        ((unsigned int)(((unsigned char *)row_pointers[y])[2 * (x * pln + p) + 1]))
								       )) / 65535.0;
	    else 
		for(y = 0; y < height; y++)
		    for (x = 0; x < width; x++)
			for (p = 0; p < pln; p++)
			    data[y + x * height + p * pls] = ((double)row_pointers[y][x * pln + p]) / 255.0;
	    dim = allocVector(INTSXP, (pln > 1) ? 3 : 2);
	    INTEGER(dim)[0] = height;
	    INTEGER(dim)[1] = width;
	    if (pln > 1)
		INTEGER(dim)[2] = pln;
	    setAttrib(res, R_DimSymbol, dim);
	    UNPROTECT(1);
	}
    }

    if (info) {
	PROTECT(res);
	setAttrib(res, install("info"), info_list);
	UNPROTECT(2);
    }
    
    png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);

    return res;
}
