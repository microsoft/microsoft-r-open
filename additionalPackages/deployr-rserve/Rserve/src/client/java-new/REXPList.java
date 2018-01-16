package org.rosuda.REngine;

/** represents a pairlist in R */
public class REXPList extends REXPVector {
	private RList payload;
	
	public REXPList(RList list) {
		super();
		payload=(list==null)?new RList():list;
	}

	public REXPList(RList list, REXPList attr) {
		super(attr);
		payload=(list==null)?new RList():list;
	}
	
	public int length() { return payload.size(); }

	public boolean isList() { return true; }
	public boolean isPairList() { return true; }

	public boolean isRecursive() { return true; }

	public RList asList() { return payload; }
	
	public String toString() {
		return super.toString()+(asList().isNamed()?"named":"");
	}
	
	public String toDebugString() {
		StringBuffer sb = new StringBuffer(super.toDebugString()+"{");
		int i = 0;
		while (i < payload.size() && i < maxDebugItems) {
			if (i>0) sb.append(",\n");
			String name = payload.keyAt(i);
			if (name!=null) sb.append(name+"=");
			sb.append(payload.at(i).toDebugString());
			i++;
		}
		if (i < payload.size()) sb.append(",..");
		return sb.toString()+"}";
	}
}
