package com.softavail.commsrouter.nexmoapp.dto.model;

import com.softavail.commsrouter.api.dto.model.ApiObjectId;

/**
 * Created by @author mapuo on 09.10.17.
 */
public class SessionDto extends ApiObjectId {

  private ApplicationDto application;
  private ExpressionDto expression;

  public SessionDto() {
  }

  public SessionDto(String id, ApplicationDto application, ExpressionDto expression) {
    super(id);
    this.application = application;
    this.expression = expression;
  }

  public ApplicationDto getApplication() {
    return application;
  }

  public void setApplication(ApplicationDto application) {
    this.application = application;
  }

  public ExpressionDto getExpression() {
    return expression;
  }

  public void setExpression(ExpressionDto expression) {
    this.expression = expression;
  }

}
