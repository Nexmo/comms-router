package com.softavail.commsrouter.nexmoapp.dto.model;

import com.softavail.commsrouter.api.dto.model.ApiObjectId;

/**
 * Created by @author mapuo on 09.10.17.
 */
public class ExpressionDto extends ApiObjectId {

  private String program;

  public ExpressionDto() {
  }

  public ExpressionDto(String id, String program) {
    super(id);
    this.program = program;
  }

  public String getProgram() {
    return program;
  }

  public void setProgram(String program) {
    this.program = program;
  }

}
