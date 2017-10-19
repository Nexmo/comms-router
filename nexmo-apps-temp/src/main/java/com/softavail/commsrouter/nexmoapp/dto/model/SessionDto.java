package com.softavail.commsrouter.nexmoapp.dto.model;

import com.softavail.commsrouter.api.dto.model.ApiObjectId;

/**
 * Created by @author mapuo on 09.10.17.
 */
public class SessionDto extends ApiObjectId {

  private ApplicationDto application;
  private ModuleDto currentModule;

  public SessionDto() {}

  public SessionDto(String id, ApplicationDto application, ModuleDto currentModule) {
    super(id);
    this.application = application;
    this.currentModule = currentModule;
  }

  public ApplicationDto getApplication() {
    return application;
  }

  public void setApplication(ApplicationDto application) {
    this.application = application;
  }

  public ModuleDto getCurrentModule() {
    return currentModule;
  }

  public void setCurrentModule(ModuleDto currentModule) {
    this.currentModule = currentModule;
  }

}
