package ru.util;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * User: dima
 * Date: 19/02/2012
 */
@Retention(RetentionPolicy.SOURCE)
public @interface Pomodoro {
    String value();
}
