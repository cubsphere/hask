//Lucas Miranda Lin - lml
public class FilaBloqueante {
	private class Node {
		Node next;
		int val;
		
		Node(int i) {
			next = null;
			val = i;
		}
	}
	
	private Node head;
	private Node tail;
	
	public FilaBloqueante() {
		head = null;
		tail = null;
	}
	
    //put ta errado
	public synchronized void put(int elem) {
		if(head == null) {
			head = new Node(elem);
			tail = head;
		} else {
			tail.next = new Node(elem);
			tail = tail.next;
		}
	}
	
	public synchronized int take() {
		int i = head.val;
		head = head.next;
		return i;
	}
}
