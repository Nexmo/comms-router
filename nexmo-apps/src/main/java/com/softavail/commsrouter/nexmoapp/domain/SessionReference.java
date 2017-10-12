/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */
package com.softavail.commsrouter.nexmoapp.domain;

import java.io.Serializable;
import java.util.Objects;
import javax.persistence.Embeddable;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 *
 * @author ikrustev
 */
@Entity
@Table(name = "session_reference")
public class SessionReference implements Serializable {

  public static enum Type {
    call, conversation, task
  }

  @Embeddable
  public static class Key {

    private Type type;
    private String value;

    @Override
    public boolean equals(Object rhsObject) {
      if (this == rhsObject) {
        return true;
      }
      if (!(rhsObject instanceof Key)) {
        return false;
      }
      Key rhs = (Key) rhsObject;
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

  private static final long serialVersionUID = 1L;
  @EmbeddedId
  private Key key;

  @ManyToOne(fetch = FetchType.EAGER)
  private Session session;

  @Override
  public boolean equals(Object rhsObject) {
    if (this == rhsObject) {
      return true;
    }
    if (!(rhsObject instanceof SessionReference)) {
      return false;
    }
    SessionReference rhs = (SessionReference) rhsObject;
    return Objects.equals(this.key, rhs.key);
  }

  @Override
  public int hashCode() {
    return key != null ? key.hashCode() : 0;
  }

  public Key getKey() {
    return key;
  }

  public void setKey(Key key) {
    this.key = key;
  }

  public Session getSession() {
    return session;
  }

  public void setSession(Session session) {
    this.session = session;
  }

}
