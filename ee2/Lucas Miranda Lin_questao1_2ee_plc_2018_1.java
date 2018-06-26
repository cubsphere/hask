//Lucas Miranda Lin - lml
import java.util.Vector;

public class VetorSeguro {
	private Vector<Integer> v;
	
	public VetorSeguro() {
		v = new Vector<Integer>();
	}
	
	public synchronized int get (int index) {
		return v.get(index);
	}
	
	public synchronized int set (int index, int element) {
		return v.set(index, element);
	}
	
	public synchronized void swap (int index1, int index2) {
		int temp = this.get(index1);
		this.set(index1, this.get(index2));
		this.set(index2, temp);
	}
}
