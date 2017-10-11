package com.softavail.commsrouter.nexmoapp.domain;

import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.domain.ApiObject;
import com.softavail.commsrouter.nexmoapp.dto.arg.CreateExpressionArg;

import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * Created by @author mapuo on 10.10.17.
 */
@Entity
@Table(name = "expression")
public class Expression extends ApiObject {

  private String program;

  public Expression() {
  }

  public Expression(CreateExpressionArg createArg, ApiObjectId objectId) {
    super(objectId.getId());
    if (createArg != null) {
      program = createArg.getProgram();
    }
  }

  public String getProgram() {
    return program;
  }

  public void setProgram(String program) {
    this.program = program;
  }

}
