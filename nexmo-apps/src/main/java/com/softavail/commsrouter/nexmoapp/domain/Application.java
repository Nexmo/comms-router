package com.softavail.commsrouter.nexmoapp.domain;

import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.domain.ApiObject;
import com.softavail.commsrouter.nexmoapp.dto.arg.CreateApplicationArg;

import java.util.List;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;

/**
 * Created by @author mapuo on 10.10.17.
 */
@Entity
@Table(name = "application")
public class Application extends ApiObject {

  @Column(name = "nexmo_app_id")
  private String nexmoAppId;

  @Column(name = "public_key")
  private String publicKey;

  @Column(name = "private_key")
  private String privateKey;

  @ManyToMany(fetch = FetchType.LAZY)
  @JoinTable(name = "application_module", joinColumns = @JoinColumn(name = "application_id"),
      inverseJoinColumns = @JoinColumn(name = "module_id"))
  private List<Module> modules;

  @OneToOne(cascade = CascadeType.ALL)
  @JoinColumn(name = "application_attribute_group_id")
  private AttributeGroup attributes;

  public Application() {}

  public Application(CreateApplicationArg createArg, ApiObjectId objectId) {
    super(objectId.getId());
    if (createArg != null) {
      nexmoAppId = createArg.getNexmoAppId();
      publicKey = createArg.getPublicKey();
      privateKey = createArg.getPrivateKey();
    }
  }

  public String getNexmoAppId() {
    return nexmoAppId;
  }

  public void setNexmoAppId(String nexmoAppId) {
    this.nexmoAppId = nexmoAppId;
  }

  public String getPublicKey() {
    return publicKey;
  }

  public void setPublicKey(String publicKey) {
    this.publicKey = publicKey;
  }

  public String getPrivateKey() {
    return privateKey;
  }

  public void setPrivateKey(String privateKey) {
    this.privateKey = privateKey;
  }

  public List<Module> getModules() {
    return modules;
  }

  public void setModules(List<Module> modules) {
    this.modules = modules;
  }

  public AttributeGroup getAttributes() {
    return attributes;
  }

  public void setAttributes(AttributeGroup attributes) {
    this.attributes = attributes;
  }

}
