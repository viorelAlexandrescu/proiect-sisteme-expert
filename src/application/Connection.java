package application;

import java.io.IOException;
import java.io.InputStream;
import java.io.PipedOutputStream;
import java.io.PrintStream;
import java.net.ServerSocket;
import java.nio.file.Path;

public class Connection {
	final String path = "C:\\Users\\Irina\\Desktop\\SICStus Prolog 4.0.2\\SICStus Prolog 4.0.2\\bin\\spwin.exe";
	final String nume_fisier = "sistem_expert_carte.pl";

	private Process expertSystemProcess;
	private int port;

	public Connection(Path path, int port, String file, String scope) throws IOException, InterruptedException {
		this.port = port;

		ServerSocket serverSocket = new ServerSocket(port);

		if (scope == null)
			scope = "inceput.";

		this.expertSystemProcess = Runtime.getRuntime()
				.exec(path.toString() + " -f -l " + file + " --goal " + scope + " -a " + port);
		
	}

	void opresteProlog() throws InterruptedException {
//		PipedOutputStream pos = this.expeditor.getPipedOutputStream();
//		PrintStream ps = new PrintStream(pos);
//		ps.println("exit.");
//		ps.flush();
	}
	
	public int getPort() {
		return this.port;
	}
}
