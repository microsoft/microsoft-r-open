package org.rosuda.REngine;

/** REXPGenericVector represents a generic vector in R. Its elements can be typically of any {@link REXP} type. */
public class REXPGenericVector extends REXPVector {
	/** payload */
	private RList payload;
	
	/** creates a new generic vector from a list. If the list is named, the <code>"names"</code> attribute is created automatically from it.
	 *  @param list list to create the vector from */
	public REXPGenericVector(RList list) {
		super();
		payload=(list==null)?new RList():list;
		// automatically generate 'names' attribute
		if (payload.isNamed())
			attr = new REXPList(
				new RList(new REXP[] { new REXPString(payload.keys()) },
						  new String[] { "names" }));
	}
	
	/** creates a new generic vector from a list. Note that the names in the list are ignored as they are expected to be defined by the attributes parameter.
	 *  @param list list to create the vector from (names are ignored - use {@link #REXPGenericVector(RList)} or the <code>"names"</code> attribute for named lists
	 *  @param attr attributes */
	public REXPGenericVector(RList list, REXPList attr) {
		super(attr);
		payload=(list==null)?new RList():list;
	}
	
	public int length() { return payload.size(); }

	public boolean isList() { return true; }

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
			sb.append(payload.at(i).toDebugString());
			i++;
		}
		if (i < payload.size()) sb.append(",..");
		return sb.toString()+"}";
	}
}
