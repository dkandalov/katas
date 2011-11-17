package cmc.game;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.ServletException;
import java.io.IOException;

import org.mortbay.jetty.Server;
import org.mortbay.jetty.handler.AbstractHandler;
import org.mortbay.jetty.Request;

public class SamplePlayer {
    public static void main(String[] args) throws Exception {
        new SamplePlayer().start(1234);
    }

    public void start(int port) throws Exception {
        Server server = new Server(port);
        server.addHandler(new AbstractHandler() {
            @Override
            public void handle(String target, HttpServletRequest request,
                               HttpServletResponse response, int dispatch) throws IOException, ServletException {
                // TODO
                ((Request) request).setHandled(true); // or false
            }
        });
        server.start();
    }
}
