package org.rosuda.JRclient;

// JRclient library - client interface to Rserve, see http://www.rosuda.org/Rserve/
// Copyright (C) 2004-06 Simon Urbanek
// --- for licensing information see LICENSE file in the original JRclient distribution ---

import java.util.*;
import java.io.*;
import java.net.*;

/**  class providing TCP/IP connection to an Rserve
     @version $Id: Rconnection.java 2737 2007-04-30 22:25:30Z urbanek $
*/
public class Rconnection {
    /** last error string */
    String lastError=null;
    Socket s;
    boolean connected=false;
    InputStream is;
    OutputStream os;
    boolean authReq=false;
    int authType=AT_plain;
    String Key=null;
    Rtalk rt=null;

    String host;
    int port;

    /** This static variable specifies the character set used to encode string for transfer. Under normal circumstances there should be no reason for changing this variable. The default is UTF-8, which makes sure that 7-bit ASCII characters are sent in a backward-compatible fashion. Currently (Rserve 0.1-7) there is no further conversion on Rserve's side, i.e. the strings are passed to R without re-coding. If necessary the setting should be changed <u>before</u> connecting to the Rserve in case later Rserves will provide a possibility of setting the encoding during the handshake. */
    public static String transferCharset="UTF-8";
    
    /** authorization type: plain text */
    public static final int AT_plain = 0;
    /** authorization type: unix crypt */
    public static final int AT_crypt = 1;

    /** version of the server (as reported in IDstring just after Rsrv) */
    protected int rsrvVersion;
    
    /** make a new local connection on default port (6311) */
    public Rconnection() throws RSrvException {
		this("127.0.0.1",6311);
    }

    /** make a new connection to specified host on default port (6311)
	@param host host name/IP
    */
    public Rconnection(String host) throws RSrvException {
		this(host,6311);
    }

    /** make a new connection to specified host and given port.
	Make sure you check {@link #isConnected} and/or {@link #isOk}.
	@param host host name/IP
	@param port TCP port
    */
    public Rconnection(String host, int port) throws RSrvException {
		this(host, port, null);
    }

    Rconnection(RSession session) throws RSrvException {
		this(null, 0, session);
    }

    Rconnection(String host, int port, RSession session) throws RSrvException {
        try {
            if (connected) s.close();
            s=null;
        } catch (Exception e) {
            throw new RSrvException(this,"Cannot connect: "+e.getMessage());
        }
		if (session!=null) {
			host=session.host;
			port=session.port;
		}
        connected=false;
		this.host=host;
		this.port=port;
        try {
            s=new Socket(host,port);
			// disable Nagle's algorithm since we really want immediate replies
			s.setTcpNoDelay(true);
        } catch (Exception sce) {
            throw new RSrvException(this,"Cannot connect: "+sce.getMessage());
        }
        try {
            is=s.getInputStream();
            os=s.getOutputStream();
        } catch (Exception gse) {
            throw new RSrvException(this,"Cannot get io stream: "+gse.getMessage());
        }
        rt=new Rtalk(is,os);
		if (session==null) {
			byte[] IDs=new byte[32];
			int n=-1;
			try {
				n=is.read(IDs);
			} catch (Exception sre) {
				throw new RSrvException(this,"Error while receiving data: "+sre.getMessage());
			}
			try {
				if (n!=32) {
					throw new RSrvException(this,"Handshake failed: expected 32 bytes header, got "+n);
				}
				String ids=new String(IDs);
				if (ids.substring(0,4).compareTo("Rsrv")!=0)
					throw new RSrvException(this,"Handshake failed: Rsrv signature expected, but received \""+ids+"\" instead.");
				try {
					rsrvVersion=Integer.parseInt(ids.substring(4,8));
				} catch (Exception px) {}
				// we support (knowingly) up to 103
				if (rsrvVersion>103)
					throw new RSrvException(this,"Handshake failed: The server uses more recent protocol than this client.");
				if (ids.substring(8,12).compareTo("QAP1")!=0)
					throw new RSrvException(this,"Handshake failed: unupported transfer protocol ("+ids.substring(8,12)+"), I talk only QAP1.");
				for (int i=12;i<32;i+=4) {
					String attr=ids.substring(i,i+4);
					if (attr.compareTo("ARpt")==0) {
						if (!authReq) { // this method is only fallback when no other was specified
							authReq=true;
							authType=AT_plain;
						}
					}
					if (attr.compareTo("ARuc")==0) {
						authReq=true;
						authType=AT_crypt;
					}
					if (attr.charAt(0)=='K') {
						Key=attr.substring(1,3);
					}
				}
			} catch (RSrvException innerX) {
				try { s.close(); } catch (Exception ex01) {}; is=null; os=null; s=null;
				throw innerX;
			}
		} else { // we have a session to take care of
			try {
				os.write(session.key,0,32);
			} catch (Exception sre) {
				throw new RSrvException(this,"Error while sending session key: "+sre.getMessage());
			}
			rsrvVersion = session.rsrvVersion;
		}
		connected=true;
		lastError="OK";
    }    
    public void finalize() {
        close();
        is=null; is=null;
    }

    /** get server version as reported during the handshake.
        @return server version as integer (Rsrv0100 will return 100) */
    public int getServerVersion() {
        return rsrvVersion;
    }
    
    /** closes current connection */
    public void close() {
        try {
            if (s!=null) s.close();
            connected=false;
        } catch(Exception e) { };
    }
    
    /** evaluates the given command, but does not fetch the result (useful for assignment
	operations)
	@param cmd command/expression string
	@return <code>true</code> if successful */
    public void voidEval(String cmd) throws RSrvException {
		if (!connected || rt==null)
			throw new RSrvException(this,"Not connected");
		Rpacket rp=rt.request(Rtalk.CMD_voidEval,cmd+"\n");
		if (rp!=null && rp.isOk()) return;
        throw new RSrvException(this,"voidEval failed",rp);
    }

	/** evaluates the given command, detaches the session (see @link{detach()}) and closes connection while the command is being evaluted (requires Rserve 0.4+).
		Note that a session cannot be attached again until the commad was successfully processed. Techincally the session is put into listening mode while the command is being evaluated but accept is called only after the command was evaluated. One commonly used techique to monitor detached working sessions is to use second connection to poll the status (e.g. create a temporary file and return the full path before detaching thus allowing new connections to read it).
		@param cmd command/expression string
		@return session object that can be use to attach back to the session once the command completed */
    public RSession voidEvalDetach(String cmd) throws RSrvException {
		if (!connected || rt==null)
			throw new RSrvException(this,"Not connected");
		Rpacket rp=rt.request(Rtalk.CMD_detachedVoidEval,cmd+"\n");
		if (rp==null || !rp.isOk())
			throw new RSrvException(this,"detached void eval failed",rp);
		RSession s = new RSession(this, rp);
		close();
		return s;
    }
	
    REXP parseEvalResponse(Rpacket rp) throws RSrvException {
		int rxo=0;
		byte[] pc=rp.getCont();
		if (rsrvVersion>100) { /* since 0101 eval responds correctly by using DT_SEXP type/len header which is 4 bytes long */
			rxo=4;
			/* we should check parameter type (should be DT_SEXP) and fail if it's not */
			if (pc[0]!=Rtalk.DT_SEXP && pc[0]!=(Rtalk.DT_SEXP|Rtalk.DT_LARGE))
				throw new RSrvException(this,"Error while processing eval output: SEXP (type "+Rtalk.DT_SEXP+") expected but found result type "+pc[0]+".");
			if (pc[0]==(Rtalk.DT_SEXP|Rtalk.DT_LARGE))
				rxo=8; // large data need skip of 8 bytes
			/* warning: we are not checking or using the length - we assume that only the one SEXP is returned. This is true for the current CMD_eval implementation, but may not be in the future. */
		}
		REXP rx=null;
		if (pc.length>rxo) {
			rx=new REXP();
			REXP.parseREXP(rx,pc,rxo);
		}
		return rx;
    }

    /** evaluates the given command and retrieves the result
	@param cmd command/expression string
	@return R-xpression or <code>null</code> if an error occured */
    public REXP eval(String cmd) throws RSrvException {
		if (!connected || rt==null)
            throw new RSrvException(this,"Not connected");
		Rpacket rp=rt.request(Rtalk.CMD_eval,cmd+"\n");
		if (rp!=null && rp.isOk())
			return parseEvalResponse(rp);
        throw new RSrvException(this,"eval failed",rp);
    }

    /** assign a string value to a symbol in R. The symbol is created if it doesn't exist already.
        @param sym symbol name. Currently assign uses CMD_setSEXP command of Rserve, i.e. the symbol value is NOT parsed. It is the responsibility of the user to make sure that the symbol name is valid in R (recall the difference between a symbol and an expression!). In fact R will always create the symbol, but it may not be accessible (examples: "bar\nfoo" or "bar$foo").
        @param ct contents
        @return <code>true</code> on success, otherwise <code>false</code>
        */
    public void assign(String sym, String ct) throws RSrvException {
		if (!connected || rt==null)
            throw new RSrvException(this,"Not connected");
        byte[] symn=sym.getBytes();
        byte[] ctn=ct.getBytes();
        int sl=symn.length+1;
        int cl=ctn.length+1;
        if ((sl&3)>0) sl=(sl&0xfffffc)+4; // make sure the symbol length is divisible by 4
        if ((cl&3)>0) cl=(cl&0xfffffc)+4; // make sure the content length is divisible by 4
        byte[] rq=new byte[sl+4+cl+4];
        int ic;
        for(ic=0;ic<symn.length;ic++) rq[ic+4]=symn[ic];
        while (ic<sl) { rq[ic+4]=0; ic++; }
        for(ic=0;ic<ctn.length;ic++) rq[ic+sl+8]=ctn[ic];
        while (ic<cl) { rq[ic+sl+8]=0; ic++; }
		Rtalk.setHdr(Rtalk.DT_STRING,sl,rq,0);
		Rtalk.setHdr(Rtalk.DT_STRING,cl,rq,sl+4);
		Rpacket rp=rt.request(Rtalk.CMD_setSEXP,rq);
        if (rp!=null && rp.isOk()) return;
        throw new RSrvException(this,"assign failed",rp);
    }

    /** assign a content of a REXP to a symbol in R. The symbol is created if it doesn't exist already.
        @param sym symbol name. Currently assign uses CMD_setSEXP command of Rserve, i.e. the symbol value is NOT parsed. It is the responsibility of the user to make sure that the symbol name is valid in R (recall the difference between a symbol and an expression!). In fact R will always create the symbol, but it may not be accessible (examples: "bar\nfoo" or "bar$foo").
        @param ct contents. currently only basic types (int, double, int[], double[]) are supported.
        @return <code>true</code> on success, otherwise <code>false</code>
        */
    public void assign(String sym, REXP r) throws RSrvException {
	if (!connected || rt==null)
	    throw new RSrvException(this,"Not connected");
	int rl=r.getBinaryLength();
        byte[] symn=sym.getBytes();
        int sl=symn.length+1;
        if ((sl&3)>0) sl=(sl&0xfffffc)+4; // make sure the symbol length is divisible by 4
        byte[] rq=new byte[sl+rl+((rl>0xfffff0)?12:8)];
        int ic;
        for(ic=0;ic<symn.length;ic++) rq[ic+4]=symn[ic];
        while(ic<sl) { rq[ic+4]=0; ic++; }; // pad with 0
		Rtalk.setHdr(Rtalk.DT_STRING,sl,rq,0);
		Rtalk.setHdr(Rtalk.DT_SEXP,rl,rq,sl+4);
        r.getBinaryRepresentation(rq,sl+((rl>0xfffff0)?12:8));
		Rpacket rp=rt.request(Rtalk.CMD_setSEXP,rq);
		if (rp!=null && rp.isOk()) return;
        throw new RSrvException(this,"assign failed",rp);
    }

    /** assign values of an array of doubles to a symbol in R (creating as vector of numbers).<br>
        equals to calling {@link #assign(String, REXP)} */        
    public void assign(String sym, double[] val) throws RSrvException {
        assign(sym,new REXP(val));
    }

    /** assign values of an array of integers to a symbol in R (creating as vector of numbers).<br>
        equals to calling {@link #assign(String, REXP)} */        
    public void assign(String sym, int[] val) throws RSrvException {
        assign(sym,new REXP(val));
    }

    /** open a file on the Rserve for reading
        @param fn file name. should not contain any path delimiters, since Rserve may restrict the access to local working directory.
        @return input stream to be used for reading. Note that the stream is read-once only, there is no support for seek or rewind. */
    public RFileInputStream openFile(String fn) throws IOException {
		return new RFileInputStream(rt,fn);
    }

    /** create a file on the Rserve for writing
        @param fn file name. should not contain any path delimiters, since Rserve may restrict the access to local working directory.
        @return output stream to be used for writinging. Note that the stream is write-once only, there is no support for seek or rewind. */
    public RFileOutputStream createFile(String fn) throws IOException {
        return new RFileOutputStream(rt,fn);
    }

    /** remove a file on the Rserve
        @param fn file name. should not contain any path delimiters, since Rserve may restrict the access to local working directory.
        @return <code>true</code> on success, <code>false</code> otherwise */
    public void removeFile(String fn) throws RSrvException {
		if (!connected || rt==null)
			throw new RSrvException(this,"Not connected");
		Rpacket rp=rt.request(Rtalk.CMD_removeFile,fn);
		if (rp!=null && rp.isOk()) return;
        throw new RSrvException(this,"removeFile failed",rp);
    }

    /** shutdown remote Rserv. Note that some Rserves cannot be shut down from
	client side (forked version). */
    public void shutdown() throws RSrvException {
		if (!connected || rt==null)
			throw new RSrvException(this,"Not connected");

		Rpacket rp=rt.request(Rtalk.CMD_shutdown);
		if (rp!=null && rp.isOk()) return;
        throw new RSrvException(this,"shutdown failed",rp);
    }

    /** Sets send buffer size of the Rserve (in bytes) for the current connection. All responses send by Rserve are stored in the send buffer before transmitting. This means that any objects you want to get from the Rserve need to fit into that buffer. By default the size of the send buffer is 2MB. If you need to receive larger objects from Rserve, you will need to use this function to enlarge the buffer. In order to save memory, you can also reduce the buffer size once it's not used anymore. Currently the buffer size is only limited by the memory available and/or 1GB (whichever is smaller). Current Rserve implementations won't go below buffer sizes of 32kb though. If the specified buffer size results in 'out of memory' on the server, the corresponding error is sent and the connection is terminated.<br>
        <i>Note:</i> This command may go away in future versions of Rserve which will use dynamic send buffer allocation.
        @param sbs send buffer size (in bytes) min=32k, max=1GB
     */
    public void setSendBufferSize(long sbs) throws RSrvException {
        if (!connected || rt==null)
			throw new RSrvException(this,"Not connected");

        Rpacket rp=rt.request(Rtalk.CMD_setBufferSize,(int)sbs);
        if (rp!=null && rp.isOk()) return;
        throw new RSrvException(this,"setSendBufferSize failed",rp);        
    }

    /** login using supplied user/pwd. Note that login must be the first
	command if used
	@param user username
	@param pwd password
	@return returns <code>true</code> on success */
    public void login(String user, String pwd) throws RSrvException {
		if (!authReq) return;
		if (!connected || rt==null)
			throw new RSrvException(this,"Not connected");
		if (authType==AT_crypt) {
			if (Key==null) Key="rs";
			Rpacket rp=rt.request(Rtalk.CMD_login,user+"\n"+jcrypt.crypt(Key,pwd));
			if (rp!=null && rp.isOk()) return;
			try { s.close(); } catch(Exception e) {};
			is=null; os=null; s=null; connected=false;
            throw new RSrvException(this,"login failed",rp);
		}
		Rpacket rp=rt.request(Rtalk.CMD_login,user+"\n"+pwd);
		if (rp!=null && rp.isOk()) return;
		try {s.close();} catch (Exception e) {};
		is=null; os=null; s=null; connected=false;
        throw new RSrvException(this,"login failed",rp);
    }

    
    /** detaches the session and closes the connection (requires Rserve 0.4+). The session can be only resumed by calling @link{RSession.attach} */
	public RSession detach() throws RSrvException {
		if (!connected || rt==null)
            throw new RSrvException(this,"Not connected");
		Rpacket rp=rt.request(Rtalk.CMD_detachSession);
		if (rp==null || !rp.isOk())
			throw new RSrvException(this,"Cannot detach",rp);
		RSession s = new RSession(this, rp);
		close();
		return s;
    }

    /** check connection state. Note that currently this state is not checked on-the-spot,
	that is if connection went down by an outside event this is not reflected by
	the flag
	@return <code>true</code> if this connection is alive */
    public boolean isConnected() { return connected; }
    
    /** check authentication requirement sent by server
	@return <code>true</code> is server requires authentication. In such case first
	command after connecting must be {@link #login}. */
    public boolean needLogin() { return authReq; }
    
    /** get last error string
	@return last error string */
    public String getLastError() { return lastError; }
}

