package katas.groovy.network.attempt1

import java.lang.annotation.Retention
import java.lang.annotation.RetentionPolicy

/**
 * User: dima
 * Date: 30/1/11
 */

@Retention(RetentionPolicy.RUNTIME)
@interface InputPort {
  String host()
  String outputPort()
}