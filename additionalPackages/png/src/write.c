#include <stdio.h>
#include <string.h>
#include <png.h>

#include <Rinternals.h>
/* for R_RED, ..., R_ALPHA */
#include <R_ext/GraphicsEngine.h>

typedef struct write_job {
    FILE *f;
    int ptr, len;
    char *data;
    SEXP rvlist, rvtail;
    int rvlen;
} write_job_t;

/* default size of a raw vector chunk when collecting the image result */
#define INIT_SIZE (1024*256)

static void user_error_fn(png_structp png_ptr, png_const_charp error_msg) {
    write_job_t *rj = (write_job_t*)png_get_error_ptr(png_ptr);
    if (rj->f) fclose(rj->f);
    Rf_error("libpng error: %s", error_msg);
}

static void user_warning_fn(png_structp png_ptr, png_const_charp warning_msg) {
    Rf_warning("libpng warning: %s", warning_msg);
}

static void user_write_data(png_structp png_ptr, png_bytep data, png_size_t length) {
    write_job_t *rj = (write_job_t*) png_get_io_ptr(png_ptr);
    png_size_t to_write = length;
    while (length) { /* use iteration instead of recursion */
	if (to_write > (rj->len - rj->ptr))
	    to_write = (rj->len - rj->ptr);
	if (to_write > 0) {
	    memcpy(rj->data + rj->ptr, data, to_write);
	    rj->ptr += to_write;
	    length -= to_write;
	    data += to_write;
	    rj->rvlen += to_write;
	}
	if (length) { /* more to go -- need next buffer */
	    SEXP rv = allocVector(RAWSXP, INIT_SIZE);
	    SETCDR(rj->rvtail, CONS(rv, R_NilValue));
	    rj->rvtail = CDR(rj->rvtail);
	    rj->len = LENGTH(rv);
	    rj->data = (char*) RAW(rv);
	    rj->ptr = 0;
	    to_write = length;
	}
    }
}

static void user_flush_data(png_structp png_ptr) {
}

#if USE_R_MALLOC
static png_voidp malloc_fn(png_structp png_ptr, png_alloc_size_t size) {
    return (png_voidp) R_alloc(1, size);
}

static void free_fn(png_structp png_ptr, png_voidp ptr) {
    /* this is a no-op because R releases the memory at the end of the call */
}
#endif

#define RX_swap32(X) (X) = (((unsigned int)(X)) >> 24) | ((((unsigned int)(X)) >> 8) & 0xff00) | (((unsigned int)(X)) << 24) | ((((unsigned int)(X)) & 0xff00) << 8)

SEXP write_png(SEXP image, SEXP sFn, SEXP sDPI, SEXP sAsp, SEXP sText) {
    SEXP res = R_NilValue, dims;
    const char *fn;
    int planes = 1, width, height, native = 0, raw_array = 0, use_dpi = 0;
    double dpi_x = 0, dpi_y = 0;
    FILE *f;
    write_job_t rj;
    png_structp png_ptr;
    png_infop info_ptr;
    
    if (inherits(image, "nativeRaster") && TYPEOF(image) == INTSXP)
	native = 1;
    
    if (TYPEOF(image) == RAWSXP)
	raw_array = 1;

    if (!native && !raw_array && TYPEOF(image) != REALSXP)
	Rf_error("image must be a matrix or array of raw or real numbers");
    
    if (TYPEOF(sDPI) == REALSXP || TYPEOF(sDPI) == INTSXP) {
	if (LENGTH(sDPI) < 1 || LENGTH(sDPI) > 2) Rf_error("invalid dpi specification - must be NULL or a numeric vector of length 1 or 2");
	if (TYPEOF(sDPI) == REALSXP) {
	    dpi_x = REAL(sDPI)[0];
	    dpi_y = (LENGTH(sDPI) > 1) ? REAL(sDPI)[1] : dpi_x;
	} else {
	    dpi_x = INTEGER(sDPI)[0];
	    dpi_y = (LENGTH(sDPI) > 1) ? INTEGER(sDPI)[1] : dpi_x;
	}
	use_dpi = 1;
    } else if (sDPI != R_NilValue)
	Rf_error("invalid `dpi' specification - must be NULL or a numeric vector of length 1 or 2");

    if (((TYPEOF(sAsp) == REALSXP || TYPEOF(sAsp) == INTSXP) && LENGTH(sAsp) != 1) ||
	(sAsp != R_NilValue && TYPEOF(sAsp) != REALSXP && TYPEOF(sAsp) != INTSXP))
	Rf_error("invalid `asp' specification - must be NULL or a numeric scalar");
    if (use_dpi && sAsp != R_NilValue)
	Rf_error("`asp' and `dpi' are mutually exclusive");
    if (sAsp != R_NilValue) {
	dpi_x = asReal(sAsp);
	dpi_y = 1.0;
	use_dpi = 2;
    }

    dims = Rf_getAttrib(image, R_DimSymbol);
    if (dims == R_NilValue || TYPEOF(dims) != INTSXP || LENGTH(dims) < 2 || LENGTH(dims) > 3)
	Rf_error("image must be a matrix or an array of two or three dimensions");

    if (raw_array && LENGTH(dims) == 3) { /* raw arrays have either bpp, width, height or width, height dimensions */
	planes = INTEGER(dims)[0];
	width = INTEGER(dims)[1];
	height = INTEGER(dims)[2];
    } else { /* others have width, height[, bpp] */
	width = INTEGER(dims)[1];
	height = INTEGER(dims)[0];
	if (LENGTH(dims) == 3)
	    planes = INTEGER(dims)[2];
    }

    if (planes < 1 || planes > 4)
	Rf_error("image must have either 1 (grayscale), 2 (GA), 3 (RGB) or 4 (RGBA) planes");

    if (native && planes > 1)
	Rf_error("native raster must be a matrix");

    if (native) { /* nativeRaster should have a "channels" attribute if it has anything else than 4 channels */
	SEXP cha = getAttrib(image, install("channels"));
	if (cha != R_NilValue) {
	    planes = asInteger(cha);
	    if (planes < 1 || planes > 4)
		planes = 4;
	} else
	    planes = 4;
    }
    if (raw_array) {
	if (planes != 4)
	    Rf_error("Only RGBA format is supported as raw data");
	native = 1; /* from now on we treat raw arrays like native */
    }

    if (TYPEOF(sFn) == RAWSXP) {
	SEXP rv = allocVector(RAWSXP, INIT_SIZE);
	rj.rvtail = rj.rvlist = PROTECT(CONS(rv, R_NilValue));
	rj.data = (char*) RAW(rv);
	rj.len = LENGTH(rv);
	rj.ptr = 0;
	rj.rvlen = 0;
	rj.f = f = 0;
    } else {
	if (TYPEOF(sFn) != STRSXP || LENGTH(sFn) < 1) Rf_error("invalid filename");
	fn = CHAR(STRING_ELT(sFn, 0));
	f = fopen(fn, "wb");
	if (!f) Rf_error("unable to create %s", fn);
	rj.f = f;
    }

    /* use our own error hanlding code and pass the fp so it can be closed on error */
    png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, (png_voidp)&rj, user_error_fn, user_warning_fn);
    if (!png_ptr) {
	if (f) fclose(f);
	Rf_error("unable to initialize libpng");
    }
    
    info_ptr = png_create_info_struct(png_ptr);
    if (!info_ptr) {
	if (f) fclose(f);
	png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
	Rf_error("unable to initialize libpng");
    }
    
    if (f)
	png_init_io(png_ptr, f);
    else
	png_set_write_fn(png_ptr, (png_voidp) &rj, user_write_data, user_flush_data);

    png_set_IHDR(png_ptr, info_ptr, width, height, 8,
		 (planes == 1) ? PNG_COLOR_TYPE_GRAY : ((planes == 2) ? PNG_COLOR_TYPE_GRAY_ALPHA : ((planes == 3) ? PNG_COLOR_TYPE_RGB : PNG_COLOR_TYPE_RGB_ALPHA)),
		 PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);

#ifdef PNG_pHYs_SUPPORTED
    if (use_dpi == 1)
	png_set_pHYs(png_ptr, info_ptr, dpi_x * 39.37008, dpi_y * 39.37008, PNG_RESOLUTION_METER);
    else if (use_dpi == 2)
	png_set_pHYs(png_ptr, info_ptr, dpi_x * 10000.0, dpi_y * 10000.0, PNG_RESOLUTION_UNKNOWN);
#else
    if (use_dpi) Rf_warning("pHYs is unsupported in your build of libpng, cannot set dpi/asp");
#endif

    if (TYPEOF(sText) == STRSXP && LENGTH(sText)) {
	SEXP nam = getAttrib(sText, R_NamesSymbol);
	int i, n = LENGTH(sText);
	{
	    png_text text_ptr[n]; /* text_ptr can be transient but the char* pointers must be valid until info is written! */
	    for (i = 0; i < n; i++) {
		text_ptr[i].compression = PNG_TEXT_COMPRESSION_NONE;
		text_ptr[i].key = (char*) ((nam == R_NilValue || i >= LENGTH(nam)) ? "" : CHAR(STRING_ELT(nam, i)));
		text_ptr[i].text = (char*) CHAR(STRING_ELT(sText, i));
	    }
	    png_set_text(png_ptr, info_ptr, text_ptr, n);
	}
    }

    {
	int rowbytes = width * planes, i;
	png_bytepp row_pointers;
	png_bytep  flat_rows;
	
	row_pointers = (png_bytepp) R_alloc(height, sizeof(png_bytep));
	flat_rows = (png_bytep) R_alloc(height, width * planes);
	for(i = 0; i < height; i++)
	    row_pointers[i] = flat_rows + (i * width * planes);
	
	if (!native) {
	    int x, y, p, pls = width * height;
	    double *data = REAL(image);
	    for(y = 0; y < height; y++)
		for (x = 0; x < width; x++)
		    for (p = 0; p < planes; p++) {
			double v = data[y + x * height + p * pls];
			if (v < 0) v = 0;
			if (v > 255.0) v = 1.0;
			row_pointers[y][x * planes + p] = (unsigned char)(v * 255.0 + 0.5);
		    }
	} else {
	    if (planes == 4) { /* 4 planes - efficient - just copy it all */
		int y, *idata = raw_array ? ((int*) RAW(image)) : INTEGER(image), need_swap = 0;
		for (y = 0; y < height; idata += width, y++)
		    memcpy(row_pointers[y], idata, width * sizeof(int));
		
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
		if (need_swap) {
		    unsigned int *idp = (unsigned int*) flat_rows, *ide = idp + (height * width);
		    for (; idp < ide; idp++)
			RX_swap32(*idp);
		}
	    } else if (planes == 3) { /* RGB */
		int x, y, *idata = INTEGER(res);
		for (y = 0; y < height; y++)
		    for (x = 0; x < rowbytes; idata++) {
			row_pointers[y][x++] = R_RED(*idata);
			row_pointers[y][x++] = R_GREEN(*idata);
			row_pointers[y][x++] = R_BLUE(*idata);
		    }
	    } else if (planes == 2) { /* GA */
		int x, y, *idata = INTEGER(res);
		for (y = 0; y < height; y++)
		    for (x = 0; x < rowbytes; idata++) {
			row_pointers[y][x++] = R_RED(*idata);
			row_pointers[y][x++] = R_ALPHA(*idata);
		    }
	    } else { /* gray */
		int x, y, *idata = INTEGER(res);
		for (y = 0; y < height; y++)
		  for (x = 0; x < rowbytes; idata++)
		    row_pointers[y][x++] = R_RED(*idata);
	    }
	}

    	png_set_rows(png_ptr, info_ptr, row_pointers);
    }

    png_write_png(png_ptr, info_ptr, PNG_TRANSFORM_IDENTITY, NULL);

    png_destroy_write_struct(&png_ptr, &info_ptr);

    if (f) { /* if it is a file, just return */
	fclose(f);
	return R_NilValue;
    }
    
    /* otherwise collect the vector blocks into one vector */
    res = allocVector(RAWSXP, rj.rvlen);
    {
	int to_go = rj.rvlen;
	unsigned char *data = RAW(res);
	while (to_go && rj.rvlist != R_NilValue) {
	    SEXP ve = CAR(rj.rvlist);
	    int this_len = (to_go > LENGTH(ve)) ? LENGTH(ve) : to_go;
	    memcpy(data, RAW(ve), this_len);
	    to_go -= this_len;
	    data += this_len;
	    rj.rvlist = CDR(rj.rvlist);
	}
    }
    
    UNPROTECT(1);
    return res;
}
