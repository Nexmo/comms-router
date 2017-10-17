package com.softavail.commsrouter.nexmoapp.dto.arg;

/**
 * Created by @author mapuo on 10.10.17.
 */
public class CreateApplicationArg {

  private String nexmoAppId;
  private String publicKey;
  private String privateKey;

  public CreateApplicationArg() {
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

}
