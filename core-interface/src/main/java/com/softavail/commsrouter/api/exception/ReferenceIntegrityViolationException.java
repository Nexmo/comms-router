package com.softavail.commsrouter.api.exception;

/**
 * Created by @author mapuo on 26.10.17.
 */
public class ReferenceIntegrityViolationException extends CommsRouterException {

  private String constraintName;

  public ReferenceIntegrityViolationException() {
  }

  public ReferenceIntegrityViolationException(String message) {
    super(message);
  }

  public ReferenceIntegrityViolationException(Throwable cause) {
    super(cause);
  }

  public ReferenceIntegrityViolationException(String message, Throwable cause) {
    super(message, cause);
  }

  public String getConstraintName() {
    return constraintName;
  }

  public void setConstraintName(String constraintName) {
    this.constraintName = constraintName;
  }

}
