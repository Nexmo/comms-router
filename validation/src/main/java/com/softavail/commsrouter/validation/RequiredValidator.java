/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.softavail.commsrouter.validation;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

/**
 *
 * @author Ergyun Syuleyman
 */
public class RequiredValidator implements ConstraintValidator<Required, Object> {

  private Required annotation;

  @Override
  public void initialize(final Required anAnnotation) {
    this.annotation = anAnnotation;
  }

  @Override
  public boolean isValid(Object value, ConstraintValidatorContext constraintContext) {
    if (value == null) {
      String msg = this.annotation.message();
      constraintContext.disableDefaultConstraintViolation();
      if (!msg.isEmpty()) {
        constraintContext.buildConstraintViolationWithTemplate("{message}")
            .addConstraintViolation();
      } else {
        constraintContext
            .buildConstraintViolationWithTemplate("{javax.validation.constraints.NotNull.message}")
            .addConstraintViolation();
      }
      return !this.annotation.mandatory();
    } else {
      String nullFields = validateFieldsForNulls(value);
      if (nullFields != null && !nullFields.isEmpty()) {
        constraintContext.disableDefaultConstraintViolation();
        constraintContext.buildConstraintViolationWithTemplate(nullFields)
            .addConstraintViolation();
        return false;
      }
    }

    return true;
  }

  public static String validateFieldsForNulls(Object objectToValidate) {
    String resultString = null;
    Field[] declaredFields = objectToValidate.getClass().getDeclaredFields();
    for (Field field : declaredFields) {
      Annotation annotation = field.getAnnotation(Required.class);
      if (annotation != null) {
        Required required = (Required) annotation;
        if (required.mandatory()) {
          field.setAccessible(true);
          try {
            if (field.get(objectToValidate) == null) {
              return (objectToValidate.getClass().getName() + "." + field.getName()
                  + " cannot be null.");
            }
          } catch (IllegalArgumentException | IllegalAccessException ex) {
            resultString = ex.getMessage();
          }
        }
      }
    }
    return resultString;
  }

}
