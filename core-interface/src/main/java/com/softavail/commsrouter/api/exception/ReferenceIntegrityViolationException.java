package com.softavail.commsrouter.api.exception;

/**
 * Created by @author mapuo on 26.10.17.
 */
public class ReferenceIntegrityViolationException extends CommsRouterException {

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

}
