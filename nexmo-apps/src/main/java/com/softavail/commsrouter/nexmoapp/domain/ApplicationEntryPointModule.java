/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */
package com.softavail.commsrouter.nexmoapp.domain;

import com.softavail.commsrouter.nexmoapp.dto.model.EntryPoint;

/**
 *
 * @author ikrustev
 */
public class ApplicationEntryPointModule {

  private Application application;
  private EntryPoint entryPoint;
  private Module module;

  public Application getApplication() {
    return application;
  }

  public void setApplication(Application application) {
    this.application = application;
  }

  public EntryPoint getEntryPoint() {
    return entryPoint;
  }

  public void setEntryPoint(EntryPoint entryPoint) {
    this.entryPoint = entryPoint;
  }

  public Module getModule() {
    return module;
  }

  public void setModule(Module module) {
    this.module = module;
  }

}
