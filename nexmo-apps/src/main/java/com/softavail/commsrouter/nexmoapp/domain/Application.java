package com.softavail.commsrouter.nexmoapp.domain;

import com.softavail.commsrouter.domain.ApiObject;

import javax.persistence.Column;
import javax.persistence.Entity;
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

}
