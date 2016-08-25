/*
 * Copyright © 2014 Samsung Electronics
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * Author: Ravi Nanjundappa <nravi.n@samsung.com>
 */

/*
 * This test exercises error scenario for over sized egl surface
 *
 */

#include "cairo-test.h"
#include <cairo-gl.h>
#include <assert.h>
#include <limits.h>

static cairo_test_status_t
preamble (cairo_test_context_t *test_ctx)
{
    EGLint rgba_attribs[] = {
	EGL_RED_SIZE, 8,
	EGL_GREEN_SIZE, 8,
	EGL_BLUE_SIZE, 8,
	EGL_ALPHA_SIZE, 8,
	EGL_SURFACE_TYPE, EGL_PBUFFER_BIT,
#if CAIRO_HAS_GL_SURFACE
	EGL_RENDERABLE_TYPE, EGL_OPENGL_BIT,
#elif CAIRO_HAS_GLESV2_SURFACE
	EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT,
#endif
	EGL_NONE
    };
    const EGLint ctx_attribs[] = {
#if CAIRO_HAS_GLESV2_SURFACE
	EGL_CONTEXT_CLIENT_VERSION, 2,
#endif
	EGL_NONE
    };

    EGLDisplay dpy;
    EGLContext ctx;
    EGLConfig config;
    EGLint numConfigs;
    int major, minor;
    cairo_device_t *device;
    cairo_surface_t *oversized_surface;
    cairo_test_status_t test_status = CAIRO_TEST_SUCCESS;

    dpy = eglGetDisplay (EGL_DEFAULT_DISPLAY);
    if (! eglInitialize (dpy, &major, &minor)) {
	test_status = CAIRO_TEST_UNTESTED;
	goto CLEANUP_1;
    }

    eglChooseConfig (dpy, rgba_attribs, &config, 1, &numConfigs);
    if (numConfigs == 0) {
	test_status = CAIRO_TEST_UNTESTED;
	goto CLEANUP_1;
    }

#if CAIRO_HAS_GL_SURFACE
    eglBindAPI (EGL_OPENGL_API);
#elif CAIRO_HAS_GLESV2_SURFACE
    eglBindAPI (EGL_OPENGL_ES_API);
#endif

   ctx = eglCreateContext (dpy, config, EGL_NO_CONTEXT,
				  ctx_attribs);
    if (ctx == EGL_NO_CONTEXT) {
	test_status = CAIRO_TEST_UNTESTED;
	goto CLEANUP_2;
    }

    device = cairo_egl_device_create (dpy, ctx);

    oversized_surface = cairo_gl_surface_create (device, CAIRO_CONTENT_COLOR_ALPHA, INT_MAX, INT_MAX);
    if (cairo_surface_status (oversized_surface) != CAIRO_STATUS_INVALID_SIZE)
        test_status = CAIRO_TEST_FAILURE;

    cairo_device_destroy (device);
    eglDestroyContext (dpy, ctx);
    eglMakeCurrent (dpy, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
    ctx = EGL_NO_CONTEXT;

CLEANUP_2:
    eglTerminate (dpy);

CLEANUP_1:
    return test_status;
}

CAIRO_TEST (egl_oversized_surface,
	    "Test that creating a surface beyond texture limits results in an error surface",
	    "egl", /* keywords */
	    NULL, /* requirements */
	    0, 0,
	    preamble, NULL)
