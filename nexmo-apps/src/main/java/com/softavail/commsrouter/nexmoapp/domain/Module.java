package com.softavail.commsrouter.nexmoapp.domain;

import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.domain.ApiObject;
import com.softavail.commsrouter.nexmoapp.dto.arg.CreateExpressionArg;
import com.softavail.commsrouter.nexmoapp.dto.model.ModuleRole;

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

  @ElementCollection(targetClass = ModuleRole.class)
  @Enumerated(EnumType.STRING)
  @CollectionTable(name = "module_roles", joinColumns = @JoinColumn(name = "module_id"))
  @Column(name = "module_role")
  private List<ModuleRole> moduleRoles;

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

  public List<ModuleRole> getModuleRoles() {
    return moduleRoles;
  }

  public void setModuleRoles(List<ModuleRole> moduleRoles) {
    this.moduleRoles = moduleRoles;
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
