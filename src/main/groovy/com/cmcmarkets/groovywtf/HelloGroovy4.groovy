package com.cmcmarkets.groovywtf

public class HelloGroovy4 {

    Closure listener

    static def main(String[] args) {
        def helloGroovy4 = new HelloGroovy4()

        helloGroovy4.say("Hello nobody")
        helloGroovy4.listener = { message ->
            println("message = ${message}")
        }
        helloGroovy4.say("Hello world")
    }

    def say(message) {
        listener?.call(message)
    }
}


