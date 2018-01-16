//
// JRclient demo - Rserve and graphics
//
// $Id: PlotDemo.java 1088 2004-08-17 16:50:00Z urbaneks $
//

import org.rosuda.JRclient.*;
import java.io.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;

// this is just a quick-and-dirty demo how to create graphics output in R and display it on the client side. It's not very efficient, but it's remote-capable. It demonstrates the use of file transfer and some error recovery techniques. Everything else is just AWT and I/O stuff

public class PlotDemo extends Canvas {
    public static void main(String args[]) {
	try {
	    Rconnection c=new Rconnection((args.length>0)?args[0]:"127.0.0.1");

            // just some demo data
            c.voidEval("data(iris); attach(iris)");
            
            // we are careful here - not all R binaries support jpeg
            // so we rather capture any failures
            REXP xp=c.eval("try(jpeg(\"test.jpg\"))");
            
            if (xp.asString()!=null) { // if there's a string then we have a problem, R sent an error
                System.out.println("Can't open jpeg graphics device:\n"+xp.asString());
                // this is analogous to 'warnings', but for us it's sufficient to get just the 1st warning
                REXP w=c.eval("if (exists(\"last.warning\") && length(last.warning)>0) names(last.warning)[1] else 0");
                if (w.asString()!=null) System.out.println(w.asString());
                return;
            }
                 
            // ok, so the device should be fine - let's plot
            c.voidEval("plot(Sepal.Length, Petal.Length, col=unclass(Species))");
            c.voidEval("dev.off()");
            
	    // the file should be ready now, so let's read (ok this isn't pretty, but hey, this ain't no beauty contest *grin* =)
            // we read in chunks of bufSize (64k by default) and store the resulting byte arrays in a vector
            // ... just in case the file gets really big ...
            // we don't know the size in advance, because it's just a stream.
            // also we can't rewind it, so we have to store it piece-by-piece
	    RFileInputStream is=c.openFile("test.jpg");
            Vector buffers=new Vector();
            int bufSize=65536;
	    byte[] buf=new byte[bufSize];
            int imgLength=0;
            int n=0;
            while (true) {
                n=is.read(buf);
                if (n==bufSize) {
                    buffers.addElement(buf);
                    buf=new byte[bufSize];
                }
                if (n>0) imgLength+=n;
                if (n<bufSize) break;
            }
            if (imgLength<10) { // this shouldn't be the case actually, beause we did some error checking, but for those paranoid ...
                System.out.println("Cannot load image, check R output, probably R didn't produce anything.");
                return;
            }
            System.out.println("The image file is "+imgLength+" bytes big.");
            
            // now let's join all the chunks into one, big array ...
            byte[] imgCode=new byte[imgLength];
            int imgPos=0;
            for (Enumeration e = buffers.elements() ; e.hasMoreElements() ;) {
                byte[] b = (byte[]) e.nextElement();
                System.arraycopy(b,0,imgCode,imgPos,bufSize);
                imgPos+=bufSize;
            }
            if (n>0) System.arraycopy(buf,0,imgCode,imgPos,n);
            
            // ... and close the file ... and remove it - we have what we need :)
	    is.close();
	    c.removeFile("test.jpg");

            // now this is pretty boring AWT stuff, nothing to do with R ...
            Image img = Toolkit.getDefaultToolkit().createImage(imgCode);
            
            Frame f = new Frame("Test image");
            f.add(new PlotDemo(img));
            f.addWindowListener(new WindowAdapter() { // just do we can close the window
                public void windowClosing(WindowEvent e) { System.exit(0); }
            });
            f.pack();
            f.setVisible(true);

	    // close Rconnection, we're done
	    c.close();
        } catch(RSrvException rse) {
            System.out.println("Rserve exception: "+rse.getMessage());
        } catch(Exception e) {
            System.out.println("Something went wrong, but it's not the Rserve: "
+e.getMessage());
            e.printStackTrace();
        }
    }

    Image img;

    public PlotDemo(Image img) {
        this.img=img;
        MediaTracker mediaTracker = new MediaTracker(this);
        mediaTracker.addImage(img, 0);
        try {
            mediaTracker.waitForID(0);
        } catch (InterruptedException ie) {
            System.err.println(ie);
            System.exit(1);
        }
        setSize(img.getWidth(null), img.getHeight(null));
    }

    public void paint(Graphics g) {
        g.drawImage(img, 0, 0, null);
    }
}
