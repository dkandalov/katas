package com.cmcmarkets.groovywtf;

public class HelloJava2 {

    private Listener listener;

    public static void main(String[] args) {
        HelloJava2 helloJava2 = new HelloJava2();

        helloJava2.say("Hello nobody");
        helloJava2.setListener(new Listener() {
            @Override
            public void onMessage(String message) {
                System.out.println("message = " + message);
            }
        });
        helloJava2.say("Hello world");
    }

    public void say(String message) {
        if (listener != null) listener.onMessage(message);
    }

    public void setListener(Listener listener) {
        this.listener = listener;
    }

    public static interface Listener {
        void onMessage(String message);
    }
}



