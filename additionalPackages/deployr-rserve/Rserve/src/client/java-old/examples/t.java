import org.rosuda.JRclient.*;
import java.io.*;
import java.util.*;

// this is just a collection of various silly things you can do when
// accessing Rserve. it's not very useful in particular, but it may
// contain some demo code

class TestException extends Exception {
    public TestException(String msg) { super(msg); }
}

public class t {
    public static void main(String args[]) {
	try {
	    Rconnection c=new Rconnection((args.length>0)?args[0]:"127.0.0.1");

	    System.out.println("ok, connected, press <enter> to continue\n");
	    System.in.read();

	    {
		System.out.println("Test assigning of lists and vectors ...");
		RList l = new RList();
		l.put("a",new REXP(new int[] { 0,1,2,3}));
		l.put("b",new REXP(new double[] { 0.5,1.2,2.3,3.0}));
		System.out.println("assign x=pairlist, y=vector, z=data.frame");
		c.assign("x", new REXP(REXP.XT_LIST_TAG, l));
		c.assign("y", new REXP(REXP.XT_VECTOR, l));
		c.assign("z", REXP.createDataFrame(l));
		System.out.println("pull all three back to Java");
		REXP x = c.eval("x");
		System.out.println("x = "+x);
		x = c.eval("y");
		System.out.println("y = "+x);
		x = c.eval("z");
		System.out.println("z = "+x);
	    }

	    { // error handling
		System.out.println("Try to evaluave an invalid expression in try...");
		String expr="foo.bar";
		REXP x = c.eval("try({"+expr+"},silent=TRUE)");
		if (x!=null) {
		    REXP cl = x.getAttribute("class");
		    if (cl != null && cl.asString().equals("try-error")) {
			//throw new RErrorException(x.asString());
			System.out.println("R reported an error: "+x.asString());
		    } else {
			System.out.println("Expression result: "+x);
		    }
		}
	    }

	    { // factors
		System.out.println("test support of factors");
		REXP f = c.eval("factor(paste('F',as.integer(runif(20)*5),sep=''))");
		System.out.println("isFactor: "+f.isFactor()+"\nasFactor: "+f.asFactor());
		if (!f.isFactor() || f.asFactor() == null) throw new TestException("factor test failed");
		System.out.println("singe-level factor used to degenerate:");
		f = c.eval("factor('foo')");
		System.out.println("isFactor: "+f.isFactor()+"\nasFactor: "+f.asFactor());
		if (!f.isFactor() || f.asFactor() == null) throw new TestException("single factor test failed");
	    }

	    // lowess example
	    System.out.println("lowess: create points");
	    double[] dataX = c.eval("rnorm(100)").asDoubleArray();
	    double[] dataY = c.eval("rnorm(100)").asDoubleArray();
	    System.out.println("lowess: assign points");
	    c.assign("x", dataX);
	    c.assign("y", dataY);
	    System.out.println("lowess: call lowess");
	    REXP xl = c.eval("lowess(x,y)");
	    // System.out.println("result = "+xl);
	    RList l = xl.asList();
	    // System.out.println("list = "+l);
	    System.out.println("lowess: retrieve points");
	    double[] lx = l.at("x").asDoubleArray();
	    double[] ly = l.at("y").asDoubleArray();

	    // matrix test
	    System.out.println("matrix: create a matrix");
	    int m=100, n=100;
	    double[] mat=new double[m*n];
	    int i=0;
	    while (i<m*n) mat[i++]=i/100;
	    System.out.println("matrix: assign a matrix");
	    c.assign("m",mat);
	    c.voidEval("m<-matrix(m,"+m+","+n+")");
	    System.out.println("matrix: cross-product");
	    double[][] mr=c.eval("crossprod(m,m)").asDoubleMatrix();

	    // I/O test
	    System.out.println("I/O test (this will fail if I/O is disabled)");
	    // create a file on the R side
	    RFileOutputStream os=c.createFile("test.txt");
	    PrintStream ps=new PrintStream(os);
	    ps.println("A\tB");
	    ps.println("1\t4");
	    ps.println("4\t6");
	    ps.close();

	    // let's read that file as a data set to prove that it's on the server
	    c.voidEval("d<-read.table(\"test.txt\",TRUE)");
	    double r=c.eval("sum(d$A)/sum(d$B)").asDouble();
	    System.out.println("sum(A)/sum(B)="+r);

	    // let's read the file back - just to see how to read files
	    RFileInputStream is=c.openFile("test.txt");
	    byte[] buf=new byte[1024];
	    System.out.println("read "+is.read(buf)+" bytes.");
	    System.out.println(new String(buf));
	    is.close();

	    // ok, we're done, so remove the file
	    // if you fail to remove the file, the entire working directory
	    // will be retained.
	    c.removeFile("test.txt");

	    // close Rconnection, we're done
	    c.close();
        } catch(RSrvException rse) {
            System.out.println("Rserve exception: "+rse.getMessage());
	} catch(TestException te) {
	    System.err.println("** Test failed: "+te.getMessage());
	    te.printStackTrace();
        } catch(Exception e) {
            System.out.println("Something went wrong, but it's not the Rserve: "
+e.getMessage());
            e.printStackTrace();
        }
    }
}
