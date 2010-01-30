package org.squeryl.tests.labo;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(RetentionPolicy.RUNTIME)
public @interface AnnotationWithClassParam {
    Class<?> optionType();
}

