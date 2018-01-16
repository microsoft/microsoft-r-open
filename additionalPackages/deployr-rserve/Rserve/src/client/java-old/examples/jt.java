import java.io.*;
import org.rosuda.JRclient.*;

/** sample class demonstrating the use of the Java interface to Rserv.<br>
    it is basically a primitive R console/object inspector
    @version $Id: jt.java 687 2004-02-01 20:40:48Z urbaneks $
*/
public class jt {
    public static void main(String[] arg) {
	try {
	    Rconnection c = new Rconnection((arg.length>0)?arg[0]:"127.0.0.1"); // make new connecton
	    System.out.println("Server vesion: "+c.getServerVersion());
	    if (c.needLogin()) { // if server requires authentication, send one
		System.out.println("authentication required.");
		c.login("guest","guest");
	    }

	    BufferedReader ir=new BufferedReader(new InputStreamReader(System.in));
	    String s=null;
	    System.out.print("> ");
	    while ((s=ir.readLine()).length()>0) {
		// this is just a quick hack to test assignments
		// if the user types: #symbol value 
		// then symbol<-value is performed
		// if no value is specified then an array of integers is assigned
		if (s.equals("shutdown")) {
		    System.out.println("Sending shutdown request");
		    c.shutdown();
		    System.out.println("Shutdown successful. Quitting console.");
		    return;
		} else		
		if (s.length()>1 && s.charAt(0)=='#') {
		    s=s.substring(1);
		    int i=s.indexOf(' ');
		    if (i<1) {
			int ti[]=new int[16];
			int j=0; while (j<16) { ti[j]=(j==0)?1:ti[j-1]*j; j++; };
			REXP r=new REXP(REXP.XT_ARRAY_INT,ti);
			//double ti[]=new double[16];
			//int j=0; while (j<16) { ti[j]=((double)j)/2; j++; };
			//REXP r=new REXP(REXP.XT_ARRAY_DOUBLE,ti);
			c.assign(s,r);
			System.out.println("assign(\""+s+"\","+r+") OK");		    
		    } else {
			c.assign(s.substring(0,i),s.substring(i+1));
			System.out.println("assign(\""+s+"\") OK");		    
		    }
		} else {
		    REXP rx=c.eval(s);
		    System.out.println("exp: "+rx.toString());
		}
		System.out.print("> ");
	    }
	} catch(RSrvException rse) {
	    System.out.println("Rserve exception: "+rse.getMessage());
	} catch(Exception e) {
	    System.out.println("Something went wrong, but it's not the Rserve: "+e.getMessage());
	    e.printStackTrace();
	}
    }
}
