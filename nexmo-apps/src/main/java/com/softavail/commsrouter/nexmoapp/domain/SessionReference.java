/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.nexmoapp.domain;

import java.io.Serializable;
import java.util.Objects;
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

  private static final long serialVersionUID = 1L;

  @EmbeddedId
  private SessionReferenceKey key;

  @ManyToOne(fetch = FetchType.EAGER)
  private Session session;

  public SessionReference() {}

  public SessionReference(SessionReferenceKey key, Session session) {
    this.key = key;
    this.session = session;
  }

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

  public SessionReferenceKey getKey() {
    return key;
  }

  public void setKey(SessionReferenceKey key) {
    this.key = key;
  }

  public Session getSession() {
    return session;
  }

  public void setSession(Session session) {
    this.session = session;
  }

}
