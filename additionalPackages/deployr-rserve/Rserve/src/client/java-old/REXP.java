package org.rosuda.JRclient;

// JRclient library - client interface to Rserve, see http://www.rosuda.org/Rserve/
// Copyright (C) 2004 Simon Urbanek
// --- for licensing information see LICENSE file in the original JRclient distribution ---

import java.util.*;

/** representation of R-eXpressions in Java

    @version $Id: REXP.java 2746 2007-05-10 17:14:14Z urbanek $
*/
public class REXP extends Object implements java.io.Serializable {
    /** xpression type: NULL */
    public static final int XT_NULL=0;
    /** xpression type: integer */
    public static final int XT_INT=1;
    /** xpression type: double */
    public static final int XT_DOUBLE=2;
    /** xpression type: String */
    public static final int XT_STR=3;
    /** xpression type: language construct (currently content is same as list) */
    public static final int XT_LANG=4;
    /** xpression type: symbol (content is symbol name: String) */
    public static final int XT_SYM=5;
    /** xpression type: RBool */    
    public static final int XT_BOOL=6;
    /** xpression type: S4 object
	@since Rserve 0.5 */
    public static final int XT_S4=7;
    /** xpression type: generic vector (RList) */
    public static final int XT_VECTOR=16;
    /** xpression type: dotted-pair list (RList) */
    public static final int XT_LIST=17;
    /** xpression type: closure (there is no java class for that type (yet?). currently the body of the closure is stored in the content part of the REXP. Please note that this may change in the future!) */
    public static final int XT_CLOS=18;
    /** xpression type: symbol name
	@since Rserve 0.5 */
    public static final int XT_SYMNAME=19;
    /** xpression type: dotted-pair list (w/o tags)
	@since Rserve 0.5 */
    public static final int XT_LIST_NOTAG=20;
    /** xpression type: dotted-pair list (w tags)
	@since Rserve 0.5 */
    public static final int XT_LIST_TAG=21;
    /** xpression type: language list (w/o tags)
	@since Rserve 0.5 */
    public static final int XT_LANG_NOTAG=22;
    /** xpression type: language list (w tags)
	@since Rserve 0.5 */
    public static final int XT_LANG_TAG=23;
    /** xpression type: expression vector */
    public static final int XT_VECTOR_EXP=26;
    /** xpression type: string vector */
    public static final int XT_VECTOR_STR=27;
    /** xpression type: int[] */
    public static final int XT_ARRAY_INT=32;
    /** xpression type: double[] */
    public static final int XT_ARRAY_DOUBLE=33;
    /** xpression type: String[] (currently not used, Vector is used instead) */
    public static final int XT_ARRAY_STR=34;
    /** internal use only! this constant should never appear in a REXP */
    public static final int XT_ARRAY_BOOL_UA=35;
    /** xpression type: RBool[] */
    public static final int XT_ARRAY_BOOL=36;
    /** xpression type: raw (byte[])
	@since Rserve 0.4-? */
    public static final int XT_RAW=37;
    /** xpression type: Complex[]
	@since Rserve 0.5 */
    public static final int XT_ARRAY_CPLX=38;
    /** xpression type: unknown; no assumptions can be made about the content */
    public static final int XT_UNKNOWN=48;

    /** xpression type: RFactor; this XT is internally generated (ergo is does not come from Rsrv.h) to support RFactor class which is built from XT_ARRAY_INT */
    public static final int XT_FACTOR=127; 

	/** used for transport only - has attribute */
	private static final int XT_HAS_ATTR=128;
	
    /** xpression type */
    int Xt;
    /** attribute xpression or <code>null</code> if none */
    REXP attr;
    /** content of the xpression - its object type is dependent of {@link #Xt} */
    Object cont;

    /** cached binary length; valid only if positive */
    long cachedBinaryLength=-1;
    
    /** construct a new, empty (NULL) expression w/o attribute */
    public REXP() { Xt=0; attr=null; cont=null; }

    /** construct a new xpression of type t and content o, but no attribute
	@param t xpression type (XT_...)
	@param o content */
    public REXP(int t, Object o) {
	Xt=t; cont=o; attr=null;
    }

    /** construct a new xpression of type t, content o and attribute at
	@param t xpression type
	@param o content
	@param at attribute */
    public REXP(int t, Object o, REXP at) {
	Xt=t; cont=o; attr=at;
    }

    /** construct a new xpression of type XT_ARRAY_DOUBLE and content val
        @param val array of doubles to store in the REXP */
    public REXP(double[] val) {
        this(XT_ARRAY_DOUBLE,val);
    }

    /** construct a new xpression of type XT_ARRAY_INT and content val
        @param val array of integers to store in the REXP */
    public REXP(int[] val) {
        this(XT_ARRAY_INT,val);
    }
    
    /** construct a new xpression of type XT_ARRAY_INT and content val
      @param val array of integers to store in the REXP */
    public REXP(String[] val) {
      this(XT_ARRAY_STR,val);
    }
    
    /** get all attributes of the REXP in the form of a list or <code>null</code> if the object has no attributes.
        @return attribute xpression or <code>null</code> if there is none associated */
    public RList getAttributes() {
        return (attr!=null)?attr.asList():null;
    }

	/** get a certain attribute
		@param name name of the attribute
		@return <code>null</code> if the attribute doesn't exist or the attribute. */
    public REXP getAttribute(String name) {
		if (attr==null || attr.cont==null) return null;
		return attr.asList().at(name);
    }

	/** set an attribute value. The attributes list is created if necessary.
		@param name attribute name
		@param value attribute value */		
	public void setAttribute(String name, REXP value) {
		if (attr==null || attr.asList() == null) attr=new REXP(XT_LIST_TAG, new RList());
		RList l = attr.asList();
		if (l == null) return;
		l.put(name, value);
	}
	
    /** get raw content. Use as... methods to retrieve contents of known type.
        @return content of the REXP
		@deprecated please use corresponding <code>as...</code> accessor methods
		*/
    public Object getContent() {
        return cont;
    }

    /** get xpression type (see XT_.. constants) of the content. It defines the type of the content object.
        @return xpression type
		@deprecated please use corresponding <code>is...</code> test methods
		*/
    public int getType() {
        return Xt;
    }

    /** return the length on the REXP. If the REXP is scalar, the length is 1, for array and vector types the length is the number of elements. For all other types the length is zero.
	@return length of the REXP */
    public int length() {
	switch (Xt) {
	case XT_INT:
	case XT_DOUBLE:
	case XT_STR:
	case XT_SYM:
	case XT_SYMNAME:
	    return 1;
	case XT_VECTOR:
	case XT_LIST:
	case XT_LIST_TAG:
	case XT_LIST_NOTAG:
	case XT_LANG_TAG:
	case XT_LANG_NOTAG:
	case XT_VECTOR_EXP:
	    return (asList()==null)?0:asList().size();
	case XT_ARRAY_INT:
	    return (cont==null)?0:((int[])cont).length;
	case XT_ARRAY_DOUBLE:
	    return (cont==null)?0:((double[])cont).length;
	case XT_ARRAY_CPLX:
	    return (cont==null)?0:((double[])cont).length/2;
	case XT_ARRAY_BOOL:
	    return (cont==null)?0:((RBool[])cont).length;
	case XT_RAW:
	    return (cont==null)?0:((byte[])cont).length;
	case XT_ARRAY_STR:
	case XT_VECTOR_STR:
	    return (cont==null)?0:((String[])cont).length;
	case XT_FACTOR:
	    return (cont==null)?0:((RFactor)cont).size();
	}
	return 0;
    }
    
    /** parses byte buffer for binary representation of xpressions - read one xpression slot (descends recursively for aggregated xpressions such as lists, vectors etc.)
	@param x xpression object to store the parsed xpression in
	@param buf buffer containing the binary representation
	@param o offset in the buffer to start at
        @return position just behind the parsed xpression. Can be use for successive calls to {@link #parseREXP} if more than one expression is stored in the binary array. */
    public static int parseREXP(REXP x, byte[] buf, int o) {
	int xl=Rtalk.getLen(buf,o);
	boolean hasAtt=((buf[o]&128)!=0);
        boolean isLong=((buf[o]&64)!=0);
	int xt=(int)(buf[o]&63);
        //System.out.println("parseREXP: type="+xt+", len="+xl+", hasAtt="+hasAtt+", isLong="+isLong);
        if (isLong) o+=4;
        o+=4;
	int eox=o+xl;
	
	x.Xt=xt; x.attr=null;
	if (hasAtt) o=parseREXP(x.attr=new REXP(),buf,o);
	if (xt==XT_NULL) {
	    x.cont=null; return o;
	};
	if (xt==XT_DOUBLE) {
	    long lr=Rtalk.getLong(buf,o);
	    x.cont=new Double(Double.longBitsToDouble(lr));
	    o+=8;
	    if (o!=eox) {
		System.err.println("Warning: double SEXP size mismatch\n");
		o=eox;
	    };
	    return o;
	}
	if (xt==XT_ARRAY_DOUBLE) {
	    int as=(eox-o)/8,i=0;
	    double[] d=new double[as];
	    while (o<eox) {
		d[i]=Double.longBitsToDouble(Rtalk.getLong(buf,o));
		o+=8;
		i++;
	    };
	    if (o!=eox) {
		System.err.println("Warning: double array SEXP size mismatch\n");
		o=eox;
	    };
	    x.cont=d;
	    return o;
	};
	if (xt==XT_BOOL) {
	    x.cont=new RBool(buf[o]); o++;
	    if (o!=eox) {
                if (eox!=o+3) // o+3 could happen if the result was aligned (1 byte data + 3 bytes padding)
                    System.err.println("Warning: bool SEXP size mismatch\n");
		o=eox;
	    };
	    return o;
	};
	if (xt==XT_ARRAY_BOOL_UA) {
	    int as=(eox-o), i=0;
            x.Xt=XT_ARRAY_BOOL; // XT_ARRAY_BOOL_UA is only old transport type for XT_ARRAY_BOOL
	    RBool[] d=new RBool[as];
	    while(o<eox) {
		d[i]=new RBool(buf[o]);
		i++; o++;
	    };
	    x.cont=d;
	    return o;
	};
        if (xt==XT_ARRAY_BOOL) {
            int as=Rtalk.getInt(buf,o);
            o+=4;
            int i=0;
            RBool[] d=new RBool[as];
            while(o<eox && i<as) {
                d[i]=new RBool(buf[o]);
                i++; o++;
            }
	    // skip the padding
	    while ((i&3)!=0) { i++; o++; };
            x.cont=d;
            return o;
        };
        if (xt==XT_INT) {
	    x.cont=new Integer(Rtalk.getInt(buf,o));
	    o+=4;
	    if (o!=eox) {
		System.err.println("Warning: int SEXP size mismatch\n");
		o=eox;
	    };
	    return o;
	}
	if (xt==XT_ARRAY_INT) {
	    int as=(eox-o)/4,i=0;
	    int[] d=new int[as];
	    while (o<eox) {
		d[i]=Rtalk.getInt(buf,o);
		o+=4;
		i++;
	    };
	    if (o!=eox) {
		System.err.println("Warning: int array SEXP size mismatch\n");
		o=eox;
	    };
	    x.cont=d;
	    // hack for lists - special lists attached to int are factors
	    if (x.attr!=null) {
		REXP ca = x.attr.asList().at("class");
		REXP ls = x.attr.asList().at("levels");
		if (ca != null && ls != null && ca.asString().equals("factor")) {
		    RFactor f=new RFactor(d, ls.asStringArray());
		    x.cont=f;
		    x.Xt=XT_FACTOR;
		    //x.attr=null;
		}
	    }
	    return o;
	}
	if (xt==XT_ARRAY_STR) {
	    int c = 0, i = o;
	    while (i < eox) if (buf[i++]==0) c++;
	    String s[] = new String[c];
	    if (c > 0) {
		c = 0; i = o;
		while (o < eox) {
		    if (buf[o]==0) {
			try {
			    s[c]=new String(buf, i, o-i, Rconnection.transferCharset);
			} catch (java.io.UnsupportedEncodingException ex) {
			    s[c]="";
			}
			c++;
			i = o + 1;
		    }
		    o++;
		}
	    }
	    x.cont = s;
	    return o;
	}

	if (xt==XT_LIST_NOTAG || xt==XT_LIST_TAG) {
	    RList l = new RList();
	    while (o<eox) {
		REXP lc=new REXP();
		String name = null;
		o=parseREXP(lc, buf, o);
		if (xt==XT_LIST_TAG) {
		    REXP ns = new REXP();
		    o=parseREXP(ns, buf, o);
		    name=ns.asString();
		}
		if (name==null) l.add(lc); else l.put(name, lc);
	    }
	    x.cont = l;
	    if (o!=eox) {
		System.err.println("Warning: int list SEXP size mismatch\n");
		o=eox;
	    }
	    return o;
	}
	if (xt==XT_VECTOR) {
	    Vector v=new Vector();
	    while(o<eox) {
		REXP xx=new REXP();
		o=parseREXP(xx,buf,o);
		v.addElement(xx);
	    };
	    if (o!=eox) {
		System.err.println("Warning: int vector SEXP size mismatch\n");
		o=eox;
	    };
	    x.cont=v;
	    // fixup for lists since they're stored as attributes of vectors
	    if (x.getAttribute("names")!=null) {
		REXP nam=x.getAttribute("names");
		RList l = new RList(v, nam.asStringArray());
		x.cont=l;
		//x.Xt=XT_LIST;
		/*
		REXP nam = l.at("names");
		l.head=((RList)x.attr.cont).head;
		l.body=new REXP(XT_VECTOR,v);
		x.cont=l;
		x.Xt=XT_LIST; x.attr=x.attr.attr;
		// one more hack: we're de-vectorizing strings if alone
		// so we should invert that in case of list heads
		if (l.head.Xt==XT_STR) {
		    Vector sv=new Vector();
		    sv.addElement(l.head);
		    l.head=new REXP(XT_VECTOR,sv,l.head.attr);
		    l.head.attr=null;
		    }; */
	    };
	    return o;
	};
	if (xt==XT_VECTOR_STR) {
	    RList v=new RList();
	    while(o<eox) {
		REXP xx=new REXP();
		o=parseREXP(xx,buf,o);
		v.addElement(xx.asString());
	    }
	    if (o!=eox) {
		System.err.println("Warning: int vector SEXP size mismatch\n");
		o=eox;
	    }
	    String sa[] = new String[v.size()];
	    int i = 0; while (i < sa.length) { sa[i]=(String)v.get(i); i++; }
	    x.cont=sa;
	    return o;
	}
	if (xt==XT_STR||xt==XT_SYMNAME) {
	    int i=o;
	    while (buf[i]!=0 && i<eox) i++;
	    try {
		x.cont=new String(buf,o,i-o,Rconnection.transferCharset);
	    } catch(Exception e) {
		System.err.println("unable to convert string\n");
		x.cont=null;
	    };
	    o=eox;
	    return o;
	};
	/*
	if (xt==XT_LIST || xt==XT_LANG) {
	    RList rl=new RList();
	    rl.head=new REXP();
	    rl.body=new REXP();
	    rl.tag=null;
	    o=parseREXP(rl.head,buf,o); // CAR
	    o=parseREXP(rl.body,buf,o); // CDR
	    if (o!=eox) {
		// if there is more data then it's presumably the TAG entry
		rl.tag=new REXP();
		o=parseREXP(rl.tag,buf,o);
		if (o!=eox) {
		    System.out.println("Warning: list SEXP size mismatch\n");
		    o=eox;
		}
	    };
	    x.cont=rl;
	    return o;
	    };*/

	if (xt==XT_SYM) {
	    REXP sym=new REXP();
	    o=parseREXP(sym,buf,o); // PRINTNAME that's all we will use
	    String s=null;
	    if (sym.Xt==XT_STR) s=(String)sym.cont; else s=sym.toString();
	    x.cont=s; // content of a symbol is its printname string (so far)
	    o=eox;
	    return o;
	}

	if (xt==XT_CLOS) {
	    REXP form=new REXP();
	    REXP body=new REXP();
	    o=parseREXP(form,buf,o);
	    o=parseREXP(body,buf,o);
	    if (o!=eox) {
		System.err.println("Warning: closure SEXP size mismatch\n");
		o=eox;
	    }
            /* curently closures are not coded into their own objects, basically due to lack of demand. */
	    x.cont=body;
	    return o;
	}

	if (xt==XT_UNKNOWN) {
	    x.cont=new Integer(Rtalk.getInt(buf,o));
	    o=eox;
	    return o;
	}

	if (xt==XT_S4) {
	    x.cont=null;
	    o=eox;
	    return o;
	}

	x.cont=null;
	o=eox;
	System.err.println("unhandled type: "+xt);
	return o;
    }

    /** Calculates the length of the binary representation of the REXP including all headers. This is the amount of memory necessary to store the REXP via {@link #getBinaryRepresentation}.
        <p>Please note that currently only XT_[ARRAY_]INT, XT_[ARRAY_]DOUBLE and XT_[ARRAY_]STR are supported! All other types will return 4 which is the size of the header.
        @return length of the REXP including headers (4 or 8 bytes)*/
    public int getBinaryLength() {
	int l=0;
	int rxt=Xt;
	if (Xt==XT_LIST || Xt==XT_LIST_TAG || Xt==XT_LIST_NOTAG)
	    rxt=(asList()!=null && asList().isNamed())?XT_LIST_TAG:XT_LIST_NOTAG;
	//System.out.print("len["+xtName(Xt)+"/"+xtName(rxt)+"] ");
	if (Xt==XT_VECTOR_STR) rxt=XT_ARRAY_STR; // VECTOR_STR is not really used
	// adjust "names" attribute for vectors
	if (Xt==XT_VECTOR && asList()!=null && asList().isNamed())
	    setAttribute("names",new REXP(asList().keys()));
	boolean hasAttr = false;
	RList al = null;
	if (attr!=null) al = attr.asList();
	if (al != null && al.size()>0) hasAttr=true;
	if (hasAttr)
	    l+=attr.getBinaryLength();
	switch (rxt) {
		case XT_NULL:
		case XT_S4:
		    break;
		case XT_INT: l+=4; break;
		case XT_DOUBLE: l+=8; break;
		case XT_STR:
		case XT_SYMNAME:
		    l+=(cont==null)?1:((String)cont).length()+1;
		    if ((l&3)>0) l=l-(l&3)+4;
		    break;
		case XT_ARRAY_INT: l+=(cont==null)?0:((int[])cont).length*4; break;
		case XT_ARRAY_DOUBLE: l+=(cont==null)?0:((double[])cont).length*8; break;
		case XT_ARRAY_CPLX: l+=(cont==null)?0:((double[])cont).length*8; break;
		case XT_LIST_TAG:
		case XT_LIST_NOTAG:
		case XT_LIST:
		case XT_VECTOR:
		    if (asList()!=null) {
			RList lst = asList();
			int i=0;
			while (i<lst.size()) {
			    REXP x = lst.at(i);
			    l += (x==null)?4:x.getBinaryLength();
			    if (rxt==XT_LIST_TAG) {
				int pl=l;
				String s = lst.keyAt(i);
				l+=4; // header for a symbol
				l+=(s==null)?1:(s.length()+1);
				if ((l&3)>0) l=l-(l&3)+4;
				// System.out.println("TAG length: "+(l-pl));
			    }
			    i++;
			}
			if ((l&3)>0) l=l-(l&3)+4;
		    }
		    break;

		case XT_VECTOR_STR:
		    if (cont!=null) {
			String sa[] = (String[])cont;
			int i = 0;
			// FIXME: this is not quite true - encoding may break this
			while (i<sa.length) {
			    l+=4; // header
			    l+=(sa[i]==null)?1:(sa[i].length()+1);
			    if ((l&3)>0) l=l-(l&3)+4;
			    i++;
			}
			break;
		    }
		case XT_ARRAY_STR:
		    if (cachedBinaryLength<0) { // if there's no cache, we have to count..
			if (cont==null) cachedBinaryLength=4; else {
			    String sa[]=(String[])cont;
			    int i=0, io=0;
			    while (i<sa.length) {
				if (sa[i]!=null) {
				    try {
					byte b[]=sa[i].getBytes(Rconnection.transferCharset);
					io+=b.length;
					b=null;
				    } catch (java.io.UnsupportedEncodingException uex) {
					// FIXME: we should so something ... so far we hope noone's gonna mess with the encoding
				    }
				}
				io++;
				i++;
			    }
			    while ((io&3)!=0) io++;
			    cachedBinaryLength=io+4;
			    if (cachedBinaryLength>0xfffff0)
				cachedBinaryLength+=4;
			}
		    }
		    return l+(int)cachedBinaryLength;
		} // switch
        if (l>0xfffff0) l+=4; // large data need 4 more bytes
	// System.out.println("len:"+(l+4)+" "+xtName(rxt)+"/"+xtName(Xt)+" "+cont);
	return l+4; // add the header
    }

    /** Stores the REXP in its binary (ready-to-send) representation including header into a buffer and returns the index of the byte behind the REXP.
        <p>Please note that currently only XT_[ARRAY_]INT, XT_[ARRAY_]DOUBLE and XT_[ARRAY_]STR are supported! All other types will be stored as SEXP of the length 0 without any contents.
        @param buf buffer to store the REXP binary into
        @param off offset of the first byte where to store the REXP
        @return the offset of the first byte behind the stored REXP */
    public int getBinaryRepresentation(byte[] buf, int off) {
	int myl=getBinaryLength();
        boolean isLarge=(myl>0xfffff0);
	boolean hasAttr = false;
	RList al = null;
	if (attr!=null) al = attr.asList();
	if (al != null && al.size()>0) hasAttr=true;
	int rxt=Xt, ooff=off;
	if (Xt==XT_VECTOR_STR) rxt=XT_ARRAY_STR; // VECTOR_STR is not really used
	if (Xt==XT_LIST || Xt==XT_LIST_TAG || Xt==XT_LIST_NOTAG)
	    rxt=(asList()!=null && asList().isNamed())?XT_LIST_TAG:XT_LIST_NOTAG;
	// System.out.println("@"+off+": "+xtName(rxt)+"/"+xtName(Xt)+" "+cont+" ("+myl+"/"+buf.length+") att="+hasAttr);
        Rtalk.setHdr(rxt|(hasAttr?XT_HAS_ATTR:0),myl-(isLarge?8:4),buf,off);
        off+=(isLarge?8:4);
	if (hasAttr) off=attr.getBinaryRepresentation(buf, off);
	switch (rxt) {
	case XT_S4:
	case XT_NULL:
	    break;
	case XT_INT: Rtalk.setInt(asInt(),buf,off); break;
	case XT_DOUBLE: Rtalk.setLong(Double.doubleToLongBits(asDouble()),buf,off); break;
	case XT_ARRAY_INT:
	    if (cont!=null) {
		int ia[]=(int[])cont;
		int i=0, io=off;
		while(i<ia.length) {
		    Rtalk.setInt(ia[i++],buf,io); io+=4;
		}
	    }
	    break;
	case XT_ARRAY_DOUBLE:
	    if (cont!=null) {
		double da[]=(double[])cont;
		int i=0, io=off;
		while(i<da.length) {
		    Rtalk.setLong(Double.doubleToLongBits(da[i++]),buf,io); io+=8;
		}
	    }
	    break;
	case XT_ARRAY_STR:
	    if (cont!=null) {
		String sa[]=(String[])cont;
		int i=0, io=off;
		while (i<sa.length) {
		    if (sa[i]!=null) {
			try {
			    byte b[]=sa[i].getBytes(Rconnection.transferCharset);
			    System.arraycopy(b,0,buf,io,b.length);
			    io+=b.length;
			    b=null;
			} catch (java.io.UnsupportedEncodingException uex) {
			    // FIXME: we should so something ... so far we hope noone's gonna mess with the encoding
			}
		    }
		    buf[io++]=0;
		    i++;
		}
		i=io-off;
		while ((i&3)!=0) { buf[io++]=1; i++; } // padding if necessary..
	    }
	    break;
	case XT_LIST_TAG:
	case XT_LIST_NOTAG:
	case XT_LIST:
	case XT_VECTOR:
	    {
		int io = off;
		if (asList()!=null) {
		    RList lst = asList();
		    int i=0;
		    while (i<lst.size()) {
			REXP x = lst.at(i);
			if (x==null) x=new REXP(XT_NULL, null);
			io = x.getBinaryRepresentation(buf, io);
			if (rxt==XT_LIST_TAG)
			    io = (new REXP(XT_SYMNAME, lst.keyAt(i))).getBinaryRepresentation(buf, io);
			i++;
		    }
		}
		// System.out.println("io="+io+", expected: "+(ooff+myl));
	    }
	    break;
	    
	case XT_VECTOR_STR:
	    if (cont!=null) {
		String sa[]=(String[])cont;
		int i=0, io=off;
		while (i<sa.length)
		    io = (new REXP(XT_STR, sa[i++])).getBinaryRepresentation(buf, io);
	    }
	    break;
	case XT_SYMNAME:
	case XT_STR:
	    getStringBinaryRepresentation(buf, off, (String)cont);
	    break;
	}
	return ooff+myl;
    }

    public static int getStringBinaryRepresentation(byte[] buf, int off, String s) {
	if (s==null) s="";
	int io=off;
	try {
	    byte b[]=s.getBytes(Rconnection.transferCharset);
	    // System.out.println("<str> @"+off+", len "+b.length+" (cont "+buf.length+") \""+s+"\"");
	    System.arraycopy(b,0,buf,io,b.length);
	    io+=b.length;
	    b=null;
	} catch (java.io.UnsupportedEncodingException uex) {
	    // FIXME: we should so something ... so far we hope noone's gonna mess with the encoding
	}
	buf[io++]=0;
	while ((io&3)!=0) buf[io++]=0; // padding if necessary..
	return io;
    }

    /** returns human-readable name of the xpression type as string. Arrays are denoted by a trailing asterisk (*).
	@param xt xpression type
	@return name of the xpression type */
    public static String xtName(int xt) {
	if (xt==XT_NULL) return "NULL";
	if (xt==XT_INT) return "INT";
	if (xt==XT_STR) return "STRING";
	if (xt==XT_DOUBLE) return "REAL";
	if (xt==XT_BOOL) return "BOOL";
	if (xt==XT_ARRAY_INT) return "INT*";
	if (xt==XT_ARRAY_STR) return "STRING*";
	if (xt==XT_ARRAY_DOUBLE) return "REAL*";
	if (xt==XT_ARRAY_BOOL) return "BOOL*";
	if (xt==XT_ARRAY_CPLX) return "COMPLEX*";
	if (xt==XT_SYM) return "SYMBOL";
	if (xt==XT_SYMNAME) return "SYMNAME";
	if (xt==XT_LANG) return "LANG";
	if (xt==XT_LIST) return "LIST";
	if (xt==XT_LIST_TAG) return "LIST+T";
	if (xt==XT_LIST_NOTAG) return "LIST/T";
	if (xt==XT_LANG_TAG) return "LANG+T";
	if (xt==XT_LANG_NOTAG) return "LANG/T";
	if (xt==XT_CLOS) return "CLOS";
	if (xt==XT_RAW) return "RAW";
	if (xt==XT_S4) return "S4";
	if (xt==XT_VECTOR) return "VECTOR";
	if (xt==XT_VECTOR_STR) return "STRING[]";
	if (xt==XT_VECTOR_EXP) return "EXPR[]";
	if (xt==XT_FACTOR) return "FACTOR";
	if (xt==XT_UNKNOWN) return "UNKNOWN";
	return "<unknown "+xt+">";
    }	

    /** get content of the REXP as string (if it is either a string or a symbol name). If the length of the character vector is more tahn one, the first element is returned.
        @return string content or <code>null</code> if the REXP is no string or symbol name*/
    public String asString() {
        return (Xt==XT_STR||Xt==XT_SYMNAME)?(String)cont:(((Xt==XT_ARRAY_STR||Xt==XT_VECTOR_STR) && cont!=null &&
					   ((String[])cont).length>0)?((String[])cont)[0]:null);
    }
    
	/** tests whether this <code>REXP</code> is a character vector (aka string array)
		@return <code>true</code> if this REXP is a character vector
		@since Rserve 0.5
		*/
	public boolean isCharacter() {
		return (Xt==XT_STR||Xt==XT_ARRAY_STR||Xt==XT_VECTOR_STR);
	}

	/** tests whether this <code>REXP</code> is a numeric vector (double or integer array or scalar)
		@return <code>true</code> if this REXP is a numeric vector
		@since Rserve 0.5
		*/
	public boolean isNumeric() {
		return (Xt==XT_ARRAY_DOUBLE||Xt==XT_ARRAY_INT||Xt==XT_DOUBLE||Xt==XT_INT);
	}
	
	/** tests whether this <code>REXP</code> is a symbol
		@return <code>true</code> if this REXP is a symbol
		@since Rserve 0.5
		*/
	public boolean isSymbol() {
		return (Xt==XT_SYM||Xt==XT_SYMNAME);
	}
	
	/** tests whether this <code>REXP</code> is NULL (either a literal nil object or the payload is <code>null</code>)
		@return <code>true</code> if this REXP is NULL
		@since Rserve 0.5
		*/
	public boolean isNull() {
		return (Xt==XT_NULL || cont==null);
	}
	
	/** tests whether this <code>REXP</code> is a logical vector (aka boolean array or scalar)
		@return <code>true</code> if this REXP is a logical vector
		@since Rserve 0.5
		*/
	public boolean isLogical() {
		return (Xt==XT_BOOL || Xt==XT_ARRAY_BOOL);
	}
	
	/** tests whether this <code>REXP</code> is a vector of complex numbers
		@return <code>true</code> if this REXP is a vector of complex numbers
		@since Rserve 0.5
		*/
	public boolean isComplex() {
		return (Xt==XT_ARRAY_CPLX);
	}
	
	/** tests whether this <code>REXP</code> is a raw vector
		@return <code>true</code> if this REXP is a raw vector
		@since Rserve 0.5
		*/
	public boolean isRaw() {
		return (Xt==XT_RAW);
	}
	
    /** get content of the REXP as an array of strings (if it is a character vector).
        @return string content or <code>null</code> if the REXP is no string or symbol name*/
    public String[] asStringArray() {
		return (Xt==XT_STR)?(new String[] { (String) cont }):((Xt==XT_ARRAY_STR||Xt==XT_VECTOR_STR)?(String[])cont:null);
    }
    
    /** get content of the REXP as int (if it is one)
        @return int content or 0 if the REXP is no integer */
    public int asInt() {
        if (Xt==XT_ARRAY_INT) {
            int i[]=(int[])cont;
            if (i!=null && i.length>0) return i[0];
        }
		if (Xt==XT_ARRAY_DOUBLE || Xt==XT_DOUBLE) return (int)asDouble();
        return (Xt==XT_INT)?((Integer)cont).intValue():0;
    }

    /** get content of the REXP as double (if it is one)
        @return double content or NaN if the REXP is no double */
    public double asDouble() {
        if (Xt==XT_ARRAY_DOUBLE) {
            double d[]=(double[])cont;
            if (d!=null && d.length>0) return d[0];
        }
        return (Xt==XT_DOUBLE)?((Double)cont).doubleValue():Double.NaN;
    }

    /** synonym for {@link asList}
        @return a list or <code>null</code>
		@deprecated use {@link asList} instead
		*/
    public RList asVector() { return asList(); }

    /** get content of the REXP as {@link RFactor} (if it is one)
        @return {@link RFactor} content or <code>null</code> if the REXP is no factor */
    public RFactor asFactor() {
        return (Xt==XT_FACTOR)?(RFactor)cont:null;
    }

	/** tests whether this <code>REXP</code> is a list (any type, i.e. dotted-pair list or generic vector)
		@return <code>true</code> if this REXP is a list */
	public boolean isList() {
		return (Xt==XT_VECTOR||Xt==XT_LIST||Xt==XT_LIST_TAG||Xt==XT_LIST_NOTAG);
	}

	/** returns <code>true</code> if the value is boolean and true
		@return <code>true</code> if the value is boolean and true */
	public boolean isTrue() {
		return (Xt==XT_BOOL && cont!=null)?((RBool)cont).isTRUE():false;
	}

	/** returns <code>true</code> if the value is boolean and false
		@return <code>true</code> if the value is boolean and false */
	public boolean isFalse() {
		return (Xt==XT_BOOL && cont!=null)?((RBool)cont).isFALSE():false;
	}
	
	/** tests whether this <code>REXP</code> is a factor
		@return <code>true</code> if this <code>REXP</code> is a factor */
	public boolean isFactor() {
		return (Xt==XT_FACTOR);
	}

    /** get content of the REXP as {@link RList} (if it is one)
        @return {@link RList} content or <code>null</code> if the REXP is no list */
    public RList asList() {
        return (Xt==XT_VECTOR||Xt==XT_LIST||Xt==XT_LIST_TAG||Xt==XT_LIST_NOTAG)?(RList)cont:null;
    }

    /** get content of the REXP as {@link RBool} (if it is one)
        @return {@link RBool} content or <code>null</code> if the REXP is no logical value */
    public RBool asBool() {
        return (Xt==XT_BOOL)?(RBool)cont:null;
    }

    /** get content of the REXP as an array of doubles. Array of integers, single double and single integer are automatically converted into such an array if necessary.
        @return double[] content or <code>null</code> if the REXP is not a array of doubles or integers */
    public double[] asDoubleArray() {
        if (Xt==XT_ARRAY_DOUBLE) return (double[])cont;
        if (Xt==XT_DOUBLE) {
            double[] d=new double[1]; d[0]=asDouble(); return d;
        }
        if (Xt==XT_INT) {
            double[] d=new double[1]; d[0]=((Integer)cont).doubleValue(); return d;
        }
        if (Xt==XT_ARRAY_INT) {
            int[] i=asIntArray();
            if (i==null) return null;
            double[] d=new double[i.length];
            int j=0;
            while (j<i.length) {
                d[j]=(double)i[j]; j++;
            }
            return d;
        }
        return null;
    }

    /** get content of the REXP as an array of integers. Unlike {@link #asDoubleArray} <u>NO</u> automatic conversion is done if the content is not an array of the correct type, because there is no canonical representation of doubles as integers. A single integer is returned as an array of the length 1.
        @return double[] content or <code>null</code> if the REXP is not a array of  integers */
    public int[] asIntArray() {
        if (Xt==XT_ARRAY_INT) return (int[])cont;
        if (Xt==XT_INT) {
            int[] i=new int[1]; i[0]=asInt(); return i;
        }
        return null;
    }

    /** returns the content of the REXP as a matrix of doubles (2D-array: m[rows][cols]). This is the same form as used by popular math packages for Java, such as JAMA. This means that following leads to desired results:<br>
        <code>Matrix m=new Matrix(c.eval("matrix(c(1,2,3,4,5,6),2,3)").asDoubleMatrix());</code>
        @return 2D array of doubles in the form double[rows][cols] or <code>null</code> if the contents is no 2-dimensional matrix of doubles */
    public double[][] asDoubleMatrix() {
        if (Xt!=XT_ARRAY_DOUBLE || attr==null || attr.Xt!=XT_LIST) return null;
        REXP dim=getAttribute("dim");
        if (dim==null || dim.Xt!=XT_ARRAY_INT) return null; // we need dimension attr
        int[] ds=dim.asIntArray();
        if (ds==null || ds.length!=2) return null; // matrix must be 2-dimensional
        int m=ds[0], n=ds[1];
        double[][] r=new double[m][n];
        double[] ct=asDoubleArray();
        if (ct==null) return null;
        // R stores matrices as matrix(c(1,2,3,4),2,2) = col1:(1,2), col2:(3,4)
        // we need to copy everything, since we create 2d array from 1d array
        int i=0,k=0;
        while (i<n) {
            int j=0;
            while (j<m) {
                r[j++][i]=ct[k++];
            }
            i++;
        }
        return r;
    }

    /** this is just an alias for {@link #asDoubleMatrix()}. */
    public double[][] asMatrix() {
        return asDoubleMatrix();
    }
    
    /** displayable contents of the expression. The expression is traversed recursively if aggregation types are used (Vector, List, etc.)
        @return String descriptive representation of the xpression */
    public String toString() {
	StringBuffer sb=
	    new StringBuffer("["+xtName(Xt)+" ");
	if (attr!=null) sb.append("\nattr="+attr+"\n ");
	if (Xt==XT_DOUBLE) sb.append((Double)cont);
	if (Xt==XT_INT) sb.append((Integer)cont);
	if (Xt==XT_BOOL) sb.append((RBool)cont);
	if (Xt==XT_FACTOR) sb.append((RFactor)cont);
	if (Xt==XT_CLOS) sb.append((REXP)cont);
	if (Xt==XT_ARRAY_DOUBLE) {
	    double[] d=(double[])cont;
	    sb.append("(");
	    for(int i=0; i<d.length; i++) {
		sb.append(d[i]);
		if (i<d.length-1) sb.append(", ");
                if (i==99) {
                    sb.append("... ("+(d.length-100)+" more values follow)");
                    break;
                }
	    };
	    sb.append(")");
	};
	if (Xt==XT_ARRAY_INT) {
	    int[] d=(int[])cont;
	    sb.append("(");
	    for(int i=0; i<d.length; i++) {
		sb.append(d[i]);
		if (i<d.length-1) sb.append(", ");
                if (i==99) {
                    sb.append("... ("+(d.length-100)+" more values follow)");
                    break;
                }
            };
	    sb.append(")");
	};
	if (Xt==XT_ARRAY_BOOL) {
	    RBool[] d=(RBool[])cont;
	    sb.append("(");
	    for(int i=0; i<d.length; i++) {
		sb.append(d[i]);
		if (i<d.length-1) sb.append(", ");
	    };
	    sb.append(")");
	};
	if (Xt==XT_VECTOR) {
	    Vector v=(Vector)cont;
	    sb.append("(");
	    for(int i=0; i<v.size(); i++) {
		sb.append(((REXP)v.elementAt(i)).toString());
		if (i<v.size()-1) sb.append(", ");
	    };
	    sb.append(")");
	};
	if (Xt==XT_STR) {
	    sb.append("\"");
	    sb.append((String)cont);
	    sb.append("\"");
	}
	if (Xt==XT_ARRAY_STR||Xt==XT_VECTOR_STR) {
	    String []s=(String[])cont;
	    if (s == null) sb.append("NULL"); else
		for(int i=0; i<s.length; i++) {
		    sb.append("\""+s[i]+"\"");
		    if (i<s.length-1) sb.append(", ");
		}
	}
	if (Xt==XT_SYM||Xt==XT_SYMNAME) {
	    sb.append((String)cont);
	};
	if (Xt==XT_LIST || Xt==XT_LANG || Xt==XT_LIST_TAG||Xt==XT_LIST_NOTAG||Xt==XT_LANG_TAG||Xt==XT_LANG_NOTAG) {
	    RList l=(RList)cont;
	    if (l==null) sb.append("NULL list"); else {
		sb.append("{");
		if (l.isNamed())
		    for(int i=0; i<l.size(); i++) {
			sb.append(l.keyAt(i)+"="+l.at(i).toString());
			if (i<l.size()-1) sb.append(", ");
		    }
		else
		    for(int i=0; i<l.size(); i++) {
			sb.append(l.at(i).toString());
			if (i<l.size()-1) sb.append(", ");
		    }
		sb.append("}");	    
	    }
	};
	if (Xt==XT_UNKNOWN) sb.append((Integer)cont);
	sb.append("]");
	return sb.toString();
    };
	
	public static String quoteString(String s) {
		// this code uses API introdiced in 1.4 so it needs to be re-written for earlier JDKs
		if (s.indexOf('\\')>=0)
			s.replaceAll("\\","\\\\");
		if (s.indexOf('"')>=0)
			s.replaceAll("\"", "\\\"");
        return "\""+s+"\"";
	}

    public static REXP createDataFrame(RList l) {
	REXP x = new REXP(XT_VECTOR, l);
	REXP fe = l.at(0);
	x.setAttribute("class", new REXP(XT_VECTOR_STR, new String[] { "data.frame" }));
	x.setAttribute("row.names", new REXP(XT_ARRAY_INT, new int[] { -2147483648, -fe.length() }));
	x.setAttribute("names", new REXP(XT_VECTOR_STR, l.keys()));
	return x;
    }
}
