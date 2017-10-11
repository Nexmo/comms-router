package com.softavail.commsrouter.nexmoapp.domain;

import com.softavail.commsrouter.nexmoapp.dto.model.EntryPoint;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.domain.ApiObject;
import com.softavail.commsrouter.nexmoapp.dto.arg.CreateExpressionArg;
import java.util.List;

import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;

/**
 * Created by @author mapuo on 10.10.17.
 */
@Entity
@Table(name = "module")
public class Module extends ApiObject {

  @Enumerated(EnumType.STRING)
  private List<EntryPoint> entryPoints;

  @Enumerated(EnumType.STRING)
  private ModuleType type;

  private String program;

  public Module() {}

  public Module(CreateExpressionArg createArg, ApiObjectId objectId) {
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
