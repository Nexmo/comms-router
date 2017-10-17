package com.softavail.commsrouter.nexmoapp.domain;

import com.softavail.commsrouter.domain.ApiObject;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 * Created by @author mapuo on 10.10.17.
 */
@Entity
@Table(name = "session")
public class Session extends ApiObject {

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "application_id")
  private Application application;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "expression_id")
  private Expression expression;

  public Application getApplication() {
    return application;
  }

  public void setApplication(Application application) {
    this.application = application;
  }

  public Expression getExpression() {
    return expression;
  }

  public void setExpression(Expression expression) {
    this.expression = expression;
  }

}
