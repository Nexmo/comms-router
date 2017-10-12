package com.softavail.commsrouter.nexmoapp.domain;

import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.domain.ApiObject;
import com.softavail.commsrouter.nexmoapp.dto.arg.CreateExpressionArg;
import com.softavail.commsrouter.nexmoapp.dto.model.EntryPoint;

import java.util.List;
import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.JoinColumn;
import javax.persistence.Table;

/**
 * Created by @author mapuo on 10.10.17.
 */
@Entity
@Table(name = "module")
public class Module extends ApiObject {

  @ElementCollection(targetClass = EntryPoint.class)
  @Enumerated(EnumType.STRING)
  @CollectionTable(name = "module_entry_points", joinColumns = @JoinColumn(name = "module_id"))
  @Column(name = "entry_points")
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

  public List<EntryPoint> getEntryPoints() {
    return entryPoints;
  }

  public void setEntryPoints(
      List<EntryPoint> entryPoints) {
    this.entryPoints = entryPoints;
  }

  public ModuleType getType() {
    return type;
  }

  public void setType(ModuleType type) {
    this.type = type;
  }

  public String getProgram() {
    return program;
  }

  public void setProgram(String program) {
    this.program = program;
  }

}
