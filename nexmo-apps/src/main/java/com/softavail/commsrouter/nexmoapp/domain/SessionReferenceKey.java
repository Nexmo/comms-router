/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */
package com.softavail.commsrouter.nexmoapp.domain;

import java.io.Serializable;
import java.util.Objects;
import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;

/**
 *
 * @author ikrustev
 */
@Embeddable
public class SessionReferenceKey implements Serializable {

  private static final long serialVersionUID = 1L;

  public static enum Type {
    call, conversation, task
  }

  @Enumerated(EnumType.STRING)
  @Column(length = 32)
  private Type type;

  private String value;

  public SessionReferenceKey() {}

  public SessionReferenceKey(Type type, String value) {
    this.type = type;
    this.value = value;
  }

  @Override
  public boolean equals(Object rhsObject) {
    if (this == rhsObject) {
      return true;
    }
    if (!(rhsObject instanceof SessionReferenceKey)) {
      return false;
    }
    SessionReferenceKey rhs = (SessionReferenceKey) rhsObject;
    return Objects.equals(this.getType(), rhs.getType())
        && Objects.equals(this.getValue(), rhs.getValue());
  }

  @Override
  public int hashCode() {
    return Objects.hash(getType(), getValue());
  }

  public Type getType() {
    return type;
  }

  public void setType(Type type) {
    this.type = type;
  }

  public String getValue() {
    return value;
  }

  public void setValue(String value) {
    this.value = value;
  }

}
