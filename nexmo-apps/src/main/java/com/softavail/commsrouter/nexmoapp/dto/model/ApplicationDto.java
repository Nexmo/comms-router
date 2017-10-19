package com.softavail.commsrouter.nexmoapp.dto.model;

import com.softavail.commsrouter.api.dto.model.ApiObjectId;

/**
 * Created by @author mapuo on 09.10.17.
 */
public class ApplicationDto extends ApiObjectId {

  private String nexmoAppId;

  private String publicKey;

  public ApplicationDto() {
  }

  public ApplicationDto(String id, String nexmoAppId, String publicKey) {
    super(id);
    this.nexmoAppId = nexmoAppId;
    this.publicKey = publicKey;
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

}
