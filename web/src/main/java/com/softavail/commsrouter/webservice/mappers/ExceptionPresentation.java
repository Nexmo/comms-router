package com.softavail.commsrouter.webservice.mappers;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

/**
 * Created by @author mapuo on 03.09.17.
 */
@JsonInclude(Include.NON_NULL)
public class ExceptionPresentation {

  class ErrorPresentation {

    public final String code;
    public final String description;

    public ErrorPresentation(String code, String description) {
      this.code = code;
      this.description = description;
    }

  }

  class HelpInformation {

    public final String url;

    HelpInformation(String url) {
      this.url = url;
    }

  }

  private final ErrorPresentation error;
  private final HelpInformation information;

  public ExceptionPresentation(Throwable exception) {
    this(exception.getClass().getSimpleName(), exception.getMessage());
  }

  public ExceptionPresentation(Throwable exception, String url) {
    this(exception.getClass().getSimpleName(), exception.getMessage(), url);
  }

  public ExceptionPresentation(String className, String message) {
    this(className, message, null);
  }

  public ExceptionPresentation(String className, String message, String url) {
    this.error = new ErrorPresentation(className, message);
    this.information = (url != null) ? new HelpInformation(url) : null;
  }

  public ErrorPresentation getError() {
    return error;
  }

  public HelpInformation getInformation() {
    return information;
  }

}
