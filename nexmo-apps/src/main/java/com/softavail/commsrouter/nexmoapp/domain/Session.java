package com.softavail.commsrouter.nexmoapp.domain;

import com.softavail.commsrouter.domain.ApiObject;
import com.softavail.commsrouter.domain.AttributeGroup;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
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
  @JoinColumn(name = "current_module_id")
  private Module currentModule;

  @OneToOne(cascade = CascadeType.ALL)
  @JoinColumn(name = "session_attribute_group_id")
  private AttributeGroup attributes;

  public Session() {}

  public Session(Application application, Module currentModule) {
    this.application = application;
    this.currentModule = currentModule;
  }

  public Application getApplication() {
    return application;
  }

  public void setApplication(Application application) {
    this.application = application;
  }

  public Module getCurrentModule() {
    return currentModule;
  }

  public void setCurrentModule(Module currentModule) {
    this.currentModule = currentModule;
  }

  public AttributeGroup getAttributes() {
    return attributes;
  }

  public void setAttributes(AttributeGroup attributes) {
    this.attributes = attributes;
  }

}
