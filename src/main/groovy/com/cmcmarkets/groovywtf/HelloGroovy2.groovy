package com.cmcmarkets.groovywtf;

public class HelloGroovy2 {

    private Listener listener;

    public static void main(String[] args) {
        HelloGroovy2 helloGroovy2 = new HelloGroovy2();

        helloGroovy2.say("Hello nobody");
        helloGroovy2.setListener(new Listener() {
            @Override
            public void onMessage(String message) {
                System.out.println("message = " + message);
            }
        });
        helloGroovy2.say("Hello world");
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


