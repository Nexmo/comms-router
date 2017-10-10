package com.softavail.commsrouter.nexmoapp.domain;

import com.softavail.commsrouter.domain.ApiObject;

import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * Created by @author mapuo on 10.10.17.
 */
@Entity
@Table(name = "expression")
public class Expression extends ApiObject {

  private String program;

  public String getProgram() {
    return program;
  }

  public void setProgram(String program) {
    this.program = program;
  }

}
