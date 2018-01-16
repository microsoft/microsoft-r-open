// JRclient library - client interface to Rserve, see http://www.rosuda.org/Rserve/
// Copyright (C) 2004 Simon Urbanek
// --- for licensing information see LICENSE file in the original JRclient distribution ---
//
//  RSrvException.java
//
//  Created by Simon Urbanek on Mon Aug 18 2003.
//
//  $Id: RSrvException.java 1949 2006-02-10 17:57:54Z urbaneks $
//

package org.rosuda.JRclient;

public class RSrvException extends Exception {
    protected Rconnection conn;
    protected String err;
    protected int reqReturnCode;

    public String getRequestErrorDescription() {
		return getRequestErrorDescription(reqReturnCode);
	}
	
    public String getRequestErrorDescription(int code) {
        switch(code) {
            case 0: return "no error";
            case 2: return "R parser: input incomplete";
            case 3: return "R parser: syntax error";
            case Rtalk.ERR_auth_failed: return "authorization failed";
            case Rtalk.ERR_conn_broken: return "connection broken";
            case Rtalk.ERR_inv_cmd: return "invalid command";
            case Rtalk.ERR_inv_par: return "invalid parameter";
            case Rtalk.ERR_IOerror: return "I/O error on the server";
            case Rtalk.ERR_not_open: return "connection is not open";
            case Rtalk.ERR_access_denied: return "access denied (local to the server)";
            case Rtalk.ERR_unsupported_cmd: return "unsupported command";
            case Rtalk.ERR_unknown_cmd: return "unknown command";
            case Rtalk.ERR_data_overflow: return "data overflow, incoming data too big";
            case Rtalk.ERR_object_too_big: return "evaluation successful, but returned object is too big to transport";
            case Rtalk.ERR_out_of_mem: return "FATAL: Rserve ran out of memory, closing connection";
			case Rtalk.ERR_session_busy: return "session is busy";
			case Rtalk.ERR_detach_failed: return "session detach failed";
        }
        return "error code: "+code;
    }

    public String getMessage() {
        return super.getMessage()+((reqReturnCode!=-1)?", request status: "+getRequestErrorDescription():"");
    }
    
    public RSrvException(Rconnection c, String msg) {
        this(c,msg,-1);
    }

    public RSrvException(Rconnection c, String msg, int requestReturnCode) {
        super(msg);
        conn=c; reqReturnCode=requestReturnCode;
		if (c!=null) c.lastError=getMessage();
    }

	public RSrvException(Rconnection c, String msg, Rpacket p) {
		this(c, msg, (p==null)?-1:p.getStat());
	}
	
    public int getRequestReturnCode() {
        return reqReturnCode;
    }
}
