/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.nexmoapp.domain;

import com.softavail.commsrouter.nexmoapp.dto.model.ModuleRole;

/**
 *
 * @author ikrustev
 */
public class ApplicationModule {

  private Application application;
  private ModuleRole moduleRole;
  private Module module;

  public Application getApplication() {
    return application;
  }

  public void setApplication(Application application) {
    this.application = application;
  }

  public ModuleRole getModuleRole() {
    return moduleRole;
  }

  public void setModuleRole(ModuleRole moduleRole) {
    this.moduleRole = moduleRole;
  }

  public Module getModule() {
    return module;
  }

  public void setModule(Module module) {
    this.module = module;
  }

}
