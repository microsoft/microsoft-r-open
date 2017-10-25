
// DO NOT EDIT THIS FILE - it is machine generated -*- c++ -*-

#ifndef __java_awt_font_MultipleMaster__
#define __java_awt_font_MultipleMaster__

#pragma interface

#include <java/lang/Object.h>
#include <gcj/array.h>

extern "Java"
{
  namespace java
  {
    namespace awt
    {
        class Font;
      namespace font
      {
          class MultipleMaster;
      }
    }
  }
}

class java::awt::font::MultipleMaster : public ::java::lang::Object
{

public:
  virtual ::java::awt::Font * deriveMMFont(JArray< jfloat > *) = 0;
  virtual ::java::awt::Font * deriveMMFont(JArray< jfloat > *, jfloat, jfloat, jfloat, jfloat) = 0;
  virtual JArray< jfloat > * getDesignAxisDefaults() = 0;
  virtual JArray< ::java::lang::String * > * getDesignAxisNames() = 0;
  virtual JArray< jfloat > * getDesignAxisRanges() = 0;
  virtual jint getNumDesignAxes() = 0;
  static ::java::lang::Class class$;
} __attribute__ ((java_interface));

#endif // __java_awt_font_MultipleMaster__