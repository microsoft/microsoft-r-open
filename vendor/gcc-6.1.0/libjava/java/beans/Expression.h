
// DO NOT EDIT THIS FILE - it is machine generated -*- c++ -*-

#ifndef __java_beans_Expression__
#define __java_beans_Expression__

#pragma interface

#include <java/beans/Statement.h>
#include <gcj/array.h>

extern "Java"
{
  namespace java
  {
    namespace beans
    {
        class Expression;
    }
  }
}

class java::beans::Expression : public ::java::beans::Statement
{

public:
  Expression(::java::lang::Object *, ::java::lang::Object *, ::java::lang::String *, JArray< ::java::lang::Object * > *);
  Expression(::java::lang::Object *, ::java::lang::String *, JArray< ::java::lang::Object * > *);
  virtual ::java::lang::Object * getValue();
  virtual void setValue(::java::lang::Object *);
  virtual ::java::lang::String * toString();
private:
  static ::java::lang::Object * UNSET;
  ::java::lang::Object * __attribute__((aligned(__alignof__( ::java::beans::Statement)))) value;
public:
  static ::java::lang::Class class$;
};

#endif // __java_beans_Expression__