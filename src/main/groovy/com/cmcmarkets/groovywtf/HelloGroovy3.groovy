package com.cmcmarkets.groovywtf

public class HelloGroovy3 {

    private Listener listener

    static void main(String[] args) {
        def helloGroovy3 = new HelloGroovy3()

        helloGroovy3.say("Hello nobody")
        helloGroovy3.listener = new Listener() {
            @Override
            public void onMessage(String message) {
                println("message = " + message)
            }
        }
        helloGroovy3.say("Hello world")
    }

    void say(String message) {
        listener?.onMessage(message)
    }

    void setListener(Listener listener) {
        this.listener = listener
    }

    interface Listener {
        void onMessage(String message)
    }
}



