//Lucas Miranda Lin - lml
import java.util.LinkedList;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

public class Padaria {
	private static final int tempoAssar = 2000;
	private static final int tempoAbastecer = 200;
	private static int paesAssados = 0;
	
	private class Pao {
		long tempoDeRetirar;
		public Pao(long tempoPosto) {
			tempoDeRetirar = tempoPosto + tempoAssar;
		}
	}
	
	private BlockingQueue<Pao> Forno;
	
	public Padaria() {
		Forno = new ArrayBlockingQueue<Pao>(50);
	}
	
	private class Assar extends Thread {
		public void run() {
			while(true) {
				//aguarda até o forno não estar vazio
				synchronized(Forno) {
					while(Forno.isEmpty()) {
						try {
							Forno.wait();
						} catch (InterruptedException e) {
							break;
						}
					}
				}
				
				//aguarda até o próximo pão assar
				Pao p = Forno.peek();
				long tempoAtual = System.currentTimeMillis();
				if (tempoAtual < p.tempoDeRetirar) {
					try {
						Thread.sleep(p.tempoDeRetirar - tempoAtual);
					} catch (InterruptedException e) {}
				}
				
				//retira um pão assado
				try {
					Forno.take();
				} catch (InterruptedException e) {}
				++paesAssados;
				if(paesAssados%10 == 0) {
					System.out.println(paesAssados + " paes assados");
				}
				
				//notifica o abastecedor quando o forno esvazia
				if(Forno.isEmpty()) {
					synchronized(Forno) {
						Forno.notify();
					}
				}
			}
		}
	}
	
	private class Abastecer extends Thread {
		public void run() {
			while(true) {
				//aguarda até o forno estar vazio
				synchronized(Forno) {
					while(!Forno.isEmpty()) {
						try {
							Forno.wait();
						} catch (InterruptedException e) {
							break;
						}
					}
				}
				
				//abastece o forno com 50 pães
				for(int i = 0; i < 5; ++i) {
					//abastece o forno com um lote de 10 pães
					LinkedList<Pao> paes = new LinkedList<Pao>();
					Long tempoPosto = System.currentTimeMillis();
					for(int j = 0; j < 10; ++j) {
						paes.add(new Pao(tempoPosto));
					}
					boolean estavaVazio = Forno.isEmpty();
					Forno.addAll(paes);
					System.out.println("forno abastecido com 10 pães");
					
					//notifica o assador quando o forno não está mais vazio
					if(estavaVazio) {
						synchronized(Forno) {
							Forno.notify();
						}
					}
					
					//aguarda o tempo de abastecimento
					try {
						Thread.sleep(tempoAbastecer);
					} catch (InterruptedException e) {}
				}
			}
		}
	}
	
	public static void main (String[] args) {
		Padaria massaNobre = new Padaria();
		new Thread(massaNobre.new Assar()).start();
		new Thread(massaNobre.new Abastecer()).start();
	}
}
