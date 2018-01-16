import java.io.*;
import org.rosuda.JRclient.*;

/** sample class demonstrating the use of the Java interface to Rserv
    @version $Id: tt.java 560 2003-10-23 14:27:49Z urbaneks $
*/
public class tt {
    public static void main(String[] arg) {
	try {
	    Rconnection c = new Rconnection((arg.length>0)?arg[0]:"127.0.0.1"); // make new connecton
	    System.out.println("Server vesion: "+c.getServerVersion());
	    if (c.needLogin()) { // if server requires authentication, send one
		System.out.println("authentication required.");
		c.login("guest","guest");
	    }

	    System.out.println("creating large data ...");
	    double[] m= new double[2*1024*1024];
	    int i=0;
	    while (i<2*1024*1024) { m[i]=i; i++; };

	    System.out.println("assigning data ...");
	    // transport the data into R
	    c.assign("m",m);
	    System.out.println("create matrix ...");
	    // create a matrix from the data
	    c.voidEval("m<-matrix(m,2048,1024)");
	    System.out.println("retireve matrix ...");
	    // just to see what you got:
	    REXP r=c.eval("m");
	    System.out.println("get as matrix ...");
	    System.out.println("got: "+r);
	    double[][] mx=r.asMatrix();
	    System.out.println("got m("+mx.length+","+mx[0].length+")");
	    
	    c.close();
	} catch(RSrvException rse) {
	    System.out.println("Rserve exception: "+rse.getMessage());
	} catch(Exception e) {
	    System.out.println("Something went wrong, but it's not the Rserve: "+e.getMessage()); e.printStackTrace();
	}
    }
}
